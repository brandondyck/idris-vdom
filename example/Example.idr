module Main

import Html

main : JS_IO ()
main =
  program $ node "div"
    [ text "Hello"
    , node "p"
      [ text "Id"
      , node "button" [text "ris"]
      ]
    , text "!"
    ]
