module HtmlElm

%default total
%access export

data Node : Type where
  TextNode : String -> Node
  ElementNode : (tag : String) -> (children : List Node) -> Node

mutual
  private
  renderList : List Node -> String
  renderList [] = ""
  renderList (x :: xs) = render x ++ renderList xs

  render : Node -> String
  render (TextNode s) = s
  render (ElementNode tag children) =
    let
      open = "<" ++ tag ++ ">"
      close = "</" ++ tag ++ ">"
      renderedChildren = renderList children
    in
      open ++ renderedChildren ++ close

Show Node where
  show = render

p : List Node -> Node
p = ElementNode "p"

div : List Node -> Node
div = ElementNode "div"

text : String -> Node
text = TextNode

doc : Node
doc =
  p [
    text "I once met a man",
    div [
      text "with a wooden leg",
      text "named Smith."
    ],
    p []
  ]
