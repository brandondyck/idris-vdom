module VirtualDOM

import VirtualDOM.DOM

%default total
%access export

data EventHandler : Type where
  On : (eventName : String) -> (handler : Ptr -> JS_IO ()) ->
       EventHandler

data Html : Type where
  HtmlElement : (tag : String) -> (events : List EventHandler) ->
                (props : List (String, String)) -> (children : List Html) ->
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
  createDOMNode (HtmlElement tag events props children) = do
    childNodes <- sequence $ createDOMNodeList children
    el <- createElement tag
    let listeners = map (\(On name listener) => (name, listener)) events
    sequence $ map (uncurry $ addEventListener el) listeners
    sequence $ map (uncurry $ setAttribute el) props
    sequence $ map (appendChild el) childNodes
    pure el
  createDOMNode (HtmlText text) = createTextNode (entitize text)

node : String -> List EventHandler -> List (String, String) -> List Html -> Html
node = HtmlElement

text : String -> Html
text = HtmlText

on : (eventName : String) -> (handler : Ptr -> JS_IO ()) -> EventHandler
on = On

partial
render : Html -> JS_IO ()
render html = do
  body <- documentBody
  setInnerHTML body ""
  rendered <- createDOMNode html
  appendChild body rendered
  pure ()
