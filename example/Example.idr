module Main

import Html

Model : Type
Model = (String, Nat)

init : Model
init = ("Hello", 5)

view : Model -> Html
view (s, n) = node "div" $ take n $ map (node "button" . pure . text) $ repeat s

main : JS_IO ()
main = program init view
