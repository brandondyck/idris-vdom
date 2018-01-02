module Test

import Html
import Specdris.Spec

%access export

main : IO ()
main = spec $ do
  describe "Html" $ do
    it "shows empty element as tag pair" $ do
      show (node "p" []) `shouldBe` "<p></p>"
    it "shows elements with the given tag names" $ do
      let el1 = show (node "a" [])
      let el2 = show (node "b" [])
      (el1 ++ el2) `shouldBe` "<a></a><b></b>"
    it "shows nested elements as nested tags" $ do
      let html = show (node "a" [node "b" []])
      html `shouldBe` "<a><b></b></a>"
    it "shows multiple child nodes in order" $ do
      let inner1 = node "b" []
      let inner2 = node "c" []
      let outer = node "a" [inner1, inner2]
      show outer `shouldBe` "<a><b></b><c></c></a>"
    it "show text node as given text" $ do
      let text1 = text "abc"
      let text2 = text "def"
      let html = show (node "p" [text1, text2])
      html `shouldBe` "<p>abcdef</p>"
