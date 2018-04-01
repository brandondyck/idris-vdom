module VirtualDOM

import VirtualDOM.DOM

%default total
%access export

data EventHandler : Type where
  On : (eventName : String) -> (handler : Ptr -> JS_IO ()) ->
       (opts : ListenerOptions) -> EventHandler

data Html : Type where
  HtmlElement : (tag : String) -> (events : List EventHandler) ->
                (attribs : List (String, String)) -> (children : List Html) ->
                Html
  HtmlText : String -> Html

entitize : String -> String
entitize s =
  let
    chars = unpack s
    replaced = foldr replaceChar [] chars
  in
    pack replaced
  where
    replacements : List (Char, List Char)
    replacements = map (map unpack)
      [ ('<', "lt")
      , ('>', "gt")
      , ('&', "amp")
      , ('\'', "apos")
      , ('"', "quot")
      ]
    replaceChar : Char -> List Char -> List Char
    replaceChar c cs =
      case lookup c replacements of
        Nothing => c :: cs
        (Just entity) => ('&' :: entity) ++ (';' :: cs)
        
updateAttribs : Node -> (old : List (String, String)) ->
              (new : List (String, String)) -> JS_IO ()
updateAttribs node old new =
  do
    traverse (removeAttribute node) (map fst old)
    traverse (uncurry $ setAttribute node) new
    pure ()

mutual
  private
  partial
  createDOMNodeList : List Html -> List (JS_IO Node)
  createDOMNodeList [] = []
  createDOMNodeList (node :: nodes) =
    createDOMNode node :: createDOMNodeList nodes

  private
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
    where
      partial
      addEventHandler : Node -> EventHandler -> JS_IO ()
      addEventHandler element (On eventName handler opts) =
        addEventListener element eventName handler opts
  createDOMNode (HtmlText text) = createTextNode (entitize text)

node : String -> List EventHandler -> List (String, String) -> List Html -> Html
node = HtmlElement

text : String -> Html
text = HtmlText

on : (eventName : String) -> (handler : Ptr -> JS_IO ()) ->
     (opts : ListenerOptions) -> EventHandler
on = On

partial
render : (root : Node) -> (old : Maybe Html) -> (new : Maybe Html) -> JS_IO ()
render root Nothing Nothing = pure ()
render root (Just oldHtml) Nothing =
  do
    nthChild root 0 >>= removeChild root
    pure ()
render root Nothing (Just html) =
  do
    createDOMNode html >>= appendChild root
    pure ()
render root (Just oldHtml) (Just newHtml) =
  do
    nthChild root 0 >>= removeChild root
    createDOMNode newHtml >>= appendChild root
    pure ()

