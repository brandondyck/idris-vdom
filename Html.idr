module Html

%default total
%access export

data Html : Type where
  HtmlElement : (tag : String) -> (children : List Html) -> Html
  HtmlText : String -> Html

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
  render (HtmlText s) = s

Show Html where
  show = render

node : String -> List Html -> Html
node = HtmlElement

text : String -> Html
text = HtmlText
