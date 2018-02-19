module Main

import VirtualDOM
import VirtualDOM.DOM

listStyle : String
listStyle = "color: green; background-color: pink"

mutual
  html1 : Html
  html1 =
    node "div" [] []
      [ node "button" [ on "click" (const main2) noOptions ] [] [ text "Toggle list" ]
      ]

  html2 : Html
  html2 =
    node "div" [] []
      [ node "button" [ on "click" (const main) noOptions ] [] [ text "Toggle list" ]
      , node "ol" [] [("style", "display:none"), ("style", listStyle)]
        [ node "li" [] [] [text "Wash the dishes"]
        , node "li" [] [] [text "Grate the carrots"]
        , node "li" [] [] [text "Investigate new extensions to Hindley-Milner"]
        ]
      ]

  main2 : JS_IO ()
  main2 = do
    body <- documentBody
    render body Nothing (Just html2)

  main : JS_IO ()
  main = do
    body <- documentBody
    render body Nothing (Just html1)
