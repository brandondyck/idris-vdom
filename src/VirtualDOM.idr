module VirtualDOM

import VirtualDOM.DOM

%default total
%access private

export
data EventHandler : Type where
  On : (eventName : String) -> (handler : Ptr -> JS_IO ()) ->
       (opts : ListenerOptions) -> EventHandler

export
data Html : Type where
  HtmlElement : (tag : String) -> (events : List EventHandler) ->
                (attribs : List (String, String)) -> (children : List Html) ->
                Html
  HtmlText : String -> Html

updateAttribs : Node -> (old : List (String, String)) ->
              (new : List (String, String)) -> JS_IO ()
updateAttribs node old new =
  do
    traverse (removeAttribute node) (map fst old)
    traverse_ (uncurry $ setAttribute node) new

partial
addEventHandler : Node -> EventHandler -> JS_IO ()
addEventHandler element (On eventName handler opts) =
  addEventListener element eventName handler opts

mutual
  partial
  createDOMNodeList : List Html -> List (JS_IO Node)
  createDOMNodeList [] = []
  createDOMNodeList (node :: nodes) =
    createDOMNode node :: createDOMNodeList nodes

  partial
  createDOMNode : Html -> JS_IO Node
  createDOMNode (HtmlElement tag events attribs children) =
    do
      childNodes <- sequence $ createDOMNodeList children
      el <- createElement tag
      traverse (addEventHandler el) events
      updateAttribs el [] attribs
      traverse (appendChild el) childNodes
      pure el
  createDOMNode (HtmlText text) = createTextNode text

export
node : String -> List EventHandler -> List (String, String) -> List Html -> Html
node = HtmlElement

export
text : String -> Html
text = HtmlText

export
on : (eventName : String) -> (handler : Ptr -> JS_IO ()) ->
     (opts : ListenerOptions) -> EventHandler
on = On

indexed : (Integral n, Enum n) => List a -> List (n, a)
indexed = zipStreamList [0..]
  where
    zipStreamList : Stream n -> List a -> List (n, a)
    zipStreamList (x :: xs) [] = []
    zipStreamList (x :: xs) (y :: ys) = (x, y) :: zipStreamList xs ys

moveChildren : (count : Nat) -> (from : Node) -> (to : Node) -> JS_IO ()
moveChildren count from to =
  for_ [0..count] $ const $
    nthChild from 0 >>= removeChild from >>= appendChild to

-- This function is much longer than I prefer.
partial
renderList : (root : Node) -> (old : List (Int, Html)) -> (new : List Html) ->
             JS_IO ()
renderList root [] [] = pure ()
renderList root [] new@(_ :: _) =
  traverse createDOMNode new >>= traverse_ (appendChild root)
renderList root old@((n, _) :: _) [] =
  traverse_ (const $ nthChild root n >>= removeChild root) old
renderList root ((n, old) :: olds) (new :: news) =
  do
    case old of
      HtmlText oldText =>
        case new of
          HtmlText newText =>
            if oldText == newText
              then pure ()
              else replaceThisChild
          HtmlElement _ _ _ _ => replaceThisChild
      HtmlElement oldTag oldEvents oldAttribs oldChildren =>
        case new of
          HtmlText _ => replaceThisChild
          HtmlElement newTag newEvents newAttribs newChildren =>
            if oldTag == newTag
              then do
                {- We can't remove listeners by what we can get from an
                   EventHandler, so instead we just swap the element for
                   a new one and transfer its children. -}
                originalElement <- nthChild root n
                finalElement <- if isCons oldEvents
                  then do
                    replacementElement <- createElement newTag
                    updateAttribs replacementElement [] newAttribs
                    {- This is really inefficient. We are potentially
                       moving these children twice. This ought to
                       be integrated with the rest of the
                       differencing render. -}
                    for oldChildren $ const $
                      nthChild originalElement 0 >>=
                        removeChild originalElement >>=
                          appendChild replacementElement
                    traverse (addEventHandler replacementElement) newEvents
                    replaceChild root replacementElement originalElement
                    pure replacementElement
                  else do
                    updateAttribs originalElement oldAttribs newAttribs
                    traverse (addEventHandler originalElement) newEvents
                    pure originalElement
                renderList finalElement (indexed oldChildren) newChildren
              else replaceThisChild
    renderList root olds news
  where
    partial
    replaceThisChild : JS_IO ()
    replaceThisChild =
      do
        oldChild <- nthChild root n
        newChild <- createDOMNode new
        replaceChild root newChild oldChild
export
partial
render : (root : Node) -> (old : Maybe Html) -> (new : Maybe Html) -> JS_IO ()
render root old new =
  renderList root (indexed $ toList old) (toList new)
-- render root Nothing Nothing = pure ()
-- render root (Just oldHtml) Nothing =
--   do
--     nthChild root 0 >>= removeChild root
--     pure ()
-- render root Nothing (Just html) =
--   do
--     createDOMNode html >>= appendChild root
--     pure ()
-- render root (Just oldHtml) (Just newHtml) =
--   do
--     nthChild root 0 >>= removeChild root
--     createDOMNode newHtml >>= appendChild root
--     pure ()
