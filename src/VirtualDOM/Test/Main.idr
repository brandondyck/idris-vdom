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

assertSelect : (shouldExist : Bool) -> (selector : String) ->
                          JS_IO SpecResult
assertSelect shouldExist selector = do
  exists <- selectorExists selector
  let reason =
    if shouldExist
      then "selector should match elements"
      else "selector should not match elements"
  if exists == shouldExist
    then pure Success
    else pure $ UnaryFailure selector reason

shouldSelect : String -> JS_IO SpecResult
shouldSelect = assertSelect True

shouldNotSelect : String -> JS_IO SpecResult
shouldNotSelect = assertSelect False

infixl 2 `collectResult`

collectResult : JS_IO SpecResult -> JS_IO SpecResult -> JS_IO SpecResult
collectResult resIO1 resIO2 = do
  res1 <- resIO1
  res2 <- resIO2
  pure $ do
    res1
    res2

singleElementIsCreated : SpecTree' FFI_JS
singleElementIsCreated =
  it "creates a single element" $ do
    let html = node "p" [] [] []
    program html
    shouldSelect "p"

nestedElementsAreCreated : SpecTree' FFI_JS
nestedElementsAreCreated =
  it "creates a nested element" $ do
    let html = node "p" [] []
                 [ node "span" [] [] []               
                 ]
    program html
    shouldSelect "p > span"

singlePropertyIsSet : SpecTree' FFI_JS
singlePropertyIsSet =
  it "sets a single property on an element" $ do
    let html = node "p" [] [("class", "testval")] []
    program html
    shouldSelect "p.testval"

duplicatePropertyOverwrites : SpecTree' FFI_JS
duplicatePropertyOverwrites =
  it "overwrites properties with duplicates" $ do
    let html = node "p" [] [ ("class", "badval")
                           , ("class", "goodval")
                           ] []
    program html
    shouldNotSelect "p.badval"
      `collectResult` shouldSelect "p.goodval"

multiplePropertiesSet : SpecTree' FFI_JS
multiplePropertiesSet =
  it "sets multiple properties on an element" $ do
    let html = node "p" [] [ ("class", "classval")
                           , ("title", "titleval")
                           ] []
    program html
    shouldSelect "p.classval[title=titleval]"

multipleElementsDifferentPropertiesSet : SpecTree' FFI_JS
multipleElementsDifferentPropertiesSet =
  it "sets different properties on multiple elements" $ do
    let html = node "div" [] [] [ node "p" [] [("class", "a")] []
                                , node "p" [] [("class", "b")] []
                                ]
    program html
    shouldSelect "p.a"
      `collectResult` shouldSelect "p.b"
      `collectResult` shouldNotSelect "p.a.b"

multipleElementsCreatedInOrder : SpecTree' FFI_JS
multipleElementsCreatedInOrder =
  it "creates elements in the given order" $ do
    let html = node "div" [] [] [ node "p" [] [] []
                                , node "span" [] [] []
                                ]
    program html
    shouldSelect "p:nth-child(1)"
      `collectResult` shouldSelect "span:nth-child(2)"

main : JS_IO ()
main = specIO' $ do
  describe "virtual DOM" $ do
    singleElementIsCreated
    nestedElementsAreCreated
    singlePropertyIsSet
    duplicatePropertyOverwrites
    multiplePropertiesSet
    multipleElementsDifferentPropertiesSet
    multipleElementsCreatedInOrder