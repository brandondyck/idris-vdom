module Main

import VirtualDOM
import VirtualDOM.DOM

lazyConst : Lazy a -> b -> a
lazyConst a b = a

listStyle : String
listStyle = "color: green; background-color: pink"

mutual
  html1 : Html
  html1 =
    node "div" [] []
      [ node "button" [ on "click" (lazyConst main2) noOptions ] [] [ text "Toggle list" ]
      ]

  html2 : Html
  html2 =
    node "div" [] []
      [ node "button" [ on "click" (lazyConst main) noOptions ] [] [ text "Toggle list" ]
      , node "ol" [] [("style", "display:none"), ("style", listStyle)]
        [ node "li" [] [] [text "Wash the dishes"]
        , node "li" [] [] [text "Grate the carrots"]
        , node "li" [] [] [text "Investigate new extensions to Hindley-Milner"]
        ]
      ]

  main2 : JS_IO ()
  main2 = documentBody >>= flip render html2

  main : JS_IO ()
  main = documentBody >>= flip render html1
