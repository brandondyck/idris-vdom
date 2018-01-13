module Main

import VirtualDOM

lazyConst : Lazy a -> b -> a
lazyConst a b = a

listStyle : String
listStyle = "color: green; background-color: pink"

mutual
  html1 : Html
  html1 =
    node "div" [] []
      [ node "button" [ on "click" $ lazyConst main2 ] [] [ text "Toggle list" ]
      ]

  html2 : Html
  html2 =
    node "div" [] []
      [ node "button" [ on "click" $ lazyConst main ] [] [ text "Toggle list" ]
      , node "ol" [] [("style", "display:none"), ("style", listStyle)]
        [ node "li" [] [] [text "Wash the dishes"]
        , node "li" [] [] [text "Grate the carrots"]
        , node "li" [] [] [text "Investigate new extensions to Hindley-Milner"]
        ]
      ]

  main2 : JS_IO ()
  main2 = program html2

  main : JS_IO ()
  main = program html1
