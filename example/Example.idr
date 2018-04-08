module Main

import VirtualDOM
import VirtualDOM.DOM

listStyle : String
listStyle = "color: green; background-color: pink"

appRoot : JS_IO Node
appRoot = getElementById "app-root"

mutual
  html1 : Html
  html1 =
    node "div" [] []
      [ node "button" [ on "click" (const main1) noOptions ] [] [ text "Toggle list" ]
      ]

  html2 : Html
  html2 =
    node "div" [] []
      [ node "span" [] []
        [ node "button" [ on "click" (const main2) noOptions ]
          [] [ text "Toggle list" ]
        ]
      , node "ol" [] [("style", "display:none"), ("style", listStyle)]
        [ node "li" [] [] [text "Wash the dishes"]
        , node "li" [] [] [text "Grate the carrots"]
        , node "li" [] [] [text "Investigate new extensions to Hindley-Milner"]
        ]
      ]

  main1 : JS_IO ()
  main1 = do
    root <- appRoot
    render root (Just html1) (Just html2)
  
  main2 : JS_IO ()
  main2 = do
    root <- appRoot
    render root (Just html2) (Just html1)

main : JS_IO ()
main = do
  root <- appRoot
  render root Nothing (Just html1)
