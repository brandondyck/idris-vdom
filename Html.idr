module Html

%default total
%access export

data Html : Type where
  HtmlElement : (tag : String) -> (children : List Html) -> Html
  HtmlText : String -> Html

private
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
  renderList : List Html -> String
  renderList [] = ""
  renderList (elmt :: elmts) = render elmt ++ renderList elmts

  private
  render : Html -> String
  render (HtmlElement tag children) =
    let
      open = "<" ++ tag ++ ">"
      close = "</" ++ tag ++ ">"
    in
      open ++ renderList children ++ close
  render (HtmlText s) = entitize s

Show Html where
  show = render

%inline
private
jscall : (name : String) -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} ->ty
jscall name ty = foreign FFI_JS name ty

private
documentBody : JS_IO Ptr
documentBody = jscall "document.body" _

private
appendChild : Ptr -> Ptr -> JS_IO Ptr
appendChild =
  jscall "(%0).appendChild(%1)" _

mutual
  private
  createDOMNodeList : List Html -> List (JS_IO Ptr)
  createDOMNodeList [] = []
  createDOMNodeList (node :: nodes) =
    createDOMNode node :: createDOMNodeList nodes

  private
  createDOMNode : Html -> JS_IO Ptr
  createDOMNode (HtmlElement tag children) = do
    childNodes <- sequence $ createDOMNodeList children
    el <- jscall "document.createElement(%0)" (String -> JS_IO Ptr) tag
    sequence $ map (appendChild el) childNodes
    pure el
  createDOMNode (HtmlText text) = do
    jscall "document.createTextNode(%0)" (String -> JS_IO Ptr) (entitize text)

node : String -> List Html -> Html
node = HtmlElement

text : String -> Html
text = HtmlText

program : (init : model) -> (view : model -> Html) -> JS_IO ()
program init view = do
  body <- documentBody
  rendered <- createDOMNode (view init)
  appendChild body rendered
  pure ()
