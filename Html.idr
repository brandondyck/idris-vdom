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

node : String -> List Html -> Html
node = HtmlElement

text : String -> Html
text = HtmlText
