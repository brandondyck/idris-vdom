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

singlePropertyIsSet : SpecTree' FFI_JS
singlePropertyIsSet =
  it "sets a single property on an element" $ do
    let html = node "p" [] [("class", "testval")] []
    program html
    pExists <- selectorExists "p.testval"
    pure $ shouldBeTrue pExists

duplicatePropertyOverwrites : SpecTree' FFI_JS
duplicatePropertyOverwrites =
  it "overwrites properties with duplicates" $ do
    let html = node "p" [] [ ("class", "badval")
                           , ("class", "goodval")
                           ] []
    program html
    badPropExists <- selectorExists "p.badval"
    goodPropExists <- selectorExists "p.goodval"
    pure $ do
      shouldBeFalse badPropExists
      shouldBeTrue goodPropExists

multiplePropertiesSet : SpecTree' FFI_JS
multiplePropertiesSet =
  it "sets multiple properties on an element" $ do
    let html = node "p" [] [ ("class", "classval")
                           , ("title", "titleval")
                           ] []
    program html
    propsExist <- selectorExists "p.classval[title=titleval]"
    pure $ shouldBeTrue propsExist

main : JS_IO ()
main = specIO' $ do
  describe "virtual DOM" $ do
    singleElementIsCreated
    nestedElementsAreCreated
    singlePropertyIsSet
    duplicatePropertyOverwrites
    multiplePropertiesSet