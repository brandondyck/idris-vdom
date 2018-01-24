module VirtualDOM.Test.Main

import Specdris.SpecIO
import VirtualDOM
import VirtualDOM.DOM

{-
Things to test:

☑ Single element is created
☑ Nested elements are created
☐ Events work on elements
☐ Properties are created on element
☐ Node is created on different root than body
-}

querySelector : String -> JS_IO Ptr
querySelector = jscall "document.querySelector(%0)" _

isNull : Ptr -> JS_IO Bool
isNull ptr = (== "true") <$>
  jscall "(%0 === null) + ''" (Ptr -> JS_IO String) ptr

selectorExists : String -> JS_IO Bool
selectorExists selector = not <$> (querySelector selector >>= isNull)

singleElementIsCreated : SpecTree' FFI_JS
singleElementIsCreated =
  it "creates a single element" $ do
    let html = node "p" [] [] []
    program html
    pExists <- selectorExists "p"
    pure $ shouldBeTrue pExists

nestedElementsAreCreated : SpecTree' FFI_JS
nestedElementsAreCreated =
  it "creates a nested element" $ do
    let html = node "p" [] []
                 [ node "span" [] [] []               
                 ]
    program html
    spanExists <- selectorExists "p > span"
    pure $ shouldBeTrue spanExists

main : JS_IO ()
main = specIO' $ do
  describe "virtual DOM" $ do
    singleElementIsCreated
    nestedElementsAreCreated
    