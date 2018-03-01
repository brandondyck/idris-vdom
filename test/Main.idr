module Main

import Specdris.SpecIO
import VirtualDOM
import VirtualDOM.DOM

{-
Things to test:

☑ Single element is created
☑ Nested elements are created
☑ Events work on elements
☑ Properties are created on element
☑ Node is created on different root than body
-}

querySelector : String -> JS_IO Ptr
querySelector = jscall "document.querySelector(%0)" _

isNull : Ptr -> JS_IO Bool
isNull ptr = (== "true") <$>
  jscall "(%0 === null) + ''" (Ptr -> JS_IO String) ptr

selectorExists : String -> JS_IO Bool
selectorExists selector = not <$> (querySelector selector >>= isNull)

dispatchEventOnId : (eventName : String) -> (elementId : String) -> JS_IO ()
dispatchEventOnId eventName elementId = do
  getElementById elementId >>= dispatchEvent eventName True
  pure ()

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

appendBodyParagraph : JS_IO ()
appendBodyParagraph = do
  body <- documentBody
  paragraph <- createElement "p"
  appendChild body paragraph
  pure ()

appendBodyParagraphWithId : String -> JS_IO ()
appendBodyParagraphWithId elementId = do
  body <- documentBody
  paragraph <- createElement "p"
  setAttribute paragraph "id" elementId
  appendChild body paragraph
  pure ()


infixl 2 `andResult`

andResult : JS_IO SpecResult -> JS_IO SpecResult -> JS_IO SpecResult
andResult resIO1 resIO2 = do
  res1 <- resIO1
  res2 <- resIO2
  pure $ do
    res1
    res2

elementsSpec : SpecTree' FFI_JS
elementsSpec =
  describe "elements" $ do
    it "creates a single element" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [] []
      shouldSelect "p"
    it "creates a nested element" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [] [ node "span" [] [] []
                               ]
      shouldSelect "p > span"
    it "creates elements in the given order" $ do
      body <- documentBody
      render body Nothing $ Just $ node "div" [] [] [ node "p" [] [] []
                                 , node "span" [] [] []
                                 ]
      shouldSelect "p:nth-child(1)"
        `andResult` shouldSelect "span:nth-child(2)"
    it "creates an element on a specified parent" $ do
      let divId = "putithere"
      body <- documentBody
      render body Nothing $ Just $ node "div" [] [("id", divId)] []
      parentDiv <- getElementById divId
      render parentDiv Nothing $ Just $ node "p" [] [] []
      shouldSelect "div > p"

propertiesSpec : SpecTree' FFI_JS
propertiesSpec =
  describe "properties" $ do
    it "sets a single property on an element" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [("class", "testval")] []
      shouldSelect "p.testval"
    it "overwrites properties with duplicate keys" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [ ("class", "badval")
                            , ("class", "goodval")
                            ] []
      shouldNotSelect "p.badval"
        `andResult` shouldSelect "p.goodval"
    it "sets multiple properties on an element" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [ ("class", "classval")
                            , ("title", "titleval")
                            ] []
      shouldSelect "p.classval[title=titleval]"
    it "sets different properties on multiple elements" $ do
      body <- documentBody
      render body Nothing $ Just $ node "div" [] [] [ node "p" [] [("class", "a")] []
                                 , node "p" [] [("class", "b")] []
                                 ]
      shouldSelect "p.a"
        `andResult` shouldSelect "p.b"
        `andResult` shouldNotSelect "p.a.b"

eventsSpec : SpecTree' FFI_JS
eventsSpec =
  describe "events" $ do
    it "executes an event handler" $ do
      body <- documentBody
      render body Nothing $ Just $ node "button"
        [on "click" (const appendBodyParagraph) noOptions] [("id", "doit")] []
      dispatchEventOnId "click" "doit"
      shouldSelect "p"
    it "does not execute a handler before dispatch" $ do
      body <- documentBody
      render body Nothing $ Just $ node "button"
        [on "click" (const appendBodyParagraph) noOptions] [("id", "doit")] []
      notThere <- shouldNotSelect "p"
      dispatchEventOnId "click" "doit"
      pure notThere `andResult` shouldSelect "p"
    it "respects the once option on listeners" $ do
      let opts = record { once = Just True } noOptions
      body <- documentBody
      render body Nothing $ Just $ node "button"
        [on "click" (const appendBodyParagraph) opts] [("id", "doit")] []
      dispatchEventOnId "click" "doit"
      dispatchEventOnId "click" "doit"
      shouldSelect "p:only-of-type"
        `andResult` shouldNotSelect "p:nth-of-type(2)"
    it "responds on capture when capture == true" $ do
      let opts = record { capture = Just True } noOptions
      body <- documentBody
      render body Nothing $ Just $ node "div"
        [ on "click" (const $ appendBodyParagraphWithId "first") opts] []
        [ node "button"
            [on "click" (const $ appendBodyParagraphWithId "second") opts]
            [("id", "doit")] []
        ]
      dispatchEventOnId "click" "doit"
      shouldSelect "p#first:nth-of-type(1)"
        `andResult` shouldSelect "p#second:nth-of-type(2)"
    it "responds on bubble when capture == false" $ do
      let opts = record { capture = Just False } noOptions
      body <- documentBody
      render body Nothing $ Just $ node "div"
        [ on "click" (const $ appendBodyParagraphWithId "second") opts] []
        [ node "div"
            [ on "click" (const $ appendBodyParagraphWithId "first") opts ]
            [("id", "doit")] []
        ]
      dispatchEventOnId "click" "doit"
      shouldSelect "p#first:nth-of-type(1)"
        `andResult` shouldSelect "p#second:nth-of-type(2)"

clearBody : JS_IO SpecResult -> JS_IO SpecResult
clearBody resultIO = do
  result <- resultIO
  body <- documentBody
  setInnerHTML body ""
  pure result

main : JS_IO ()
main = specIO' {around = clearBody} $ do
  describe "initial rendering" $ do
    elementsSpec
    propertiesSpec
    eventsSpec
  describe "subsequent rendering" $ do
    it "leaves empty DOM empty when old and new virtual DOMs are Nothing" $ do
      body <- documentBody
      precondition <- shouldNotSelect "body *"
      render body Nothing Nothing
      pure precondition `andResult` shouldNotSelect "body *"
    it "empties DOM when old virtual DOM is Just and new one is Nothing" $ do
      body <- documentBody
      let html = node "div" [] [] [ node "p" [] [] [] ]
      render body Nothing (Just html)
      precondition <- shouldSelect "body > div > p"
      render body (Just html) Nothing
      pure precondition `andResult` shouldNotSelect "body *"
    it "replaces DOM elements when old and new virtual DOMs are Just" $
      let
        oldHtml = Just $ node "div" [] []
          [ node "p" [] [] [ text "paragraph 1" ]
          , node "p" [] [] [ text "paragraph 2" ]
          ]
        newHtml = Just $ node "p" [] []
          [ node "div" [] [] [ text "div 1" ]
          , node "div" [] [] [ text "div 2" ]
          ]
      in do
        body <- documentBody
        render body Nothing oldHtml
        render body oldHtml newHtml
        shouldNotSelect "body > div"
          `andResult` shouldSelect "body > p > div:nth-of-type(2)"
    it "removes element attributes when they are in old virtual DOM and not in new" $
      let
        oldHtml = Just $ node "div" []
          [ ("style", "color:red")
          , ("title", "The div")
          ] []
        newHtml = Just $ node "div" []
          [ ("title", "The div") ] []
      in do
        body <- documentBody
        render body Nothing oldHtml
        precondition <- shouldSelect "body > div[style][title]"
        render body oldHtml newHtml
        pure precondition
          `andResult` shouldNotSelect "body > div[style][title]"
          `andResult` shouldSelect "body > div[title]"
    it "adds element attributes when they are in new virtual DOM and not in old" $
      let
        oldHtml = Just $ node "div" []
          [ ("title", "The div") ] []
        newHtml = Just $ node "div" []
          [ ("style", "color:red")
          , ("title", "The div")
          ] []
      in do
        body <- documentBody
        render body Nothing oldHtml
        precondition <- shouldNotSelect "body > div[style][title]"
          `andResult` shouldSelect "body > div[title]"
        render body oldHtml newHtml
        pure precondition
          `andResult` shouldSelect "body > div[style][title]"
