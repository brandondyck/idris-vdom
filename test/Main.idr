module Main

import Specdris.SpecIO
import VirtualDOM
import VirtualDOM.DOM

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
      para1st <- shouldSelect "p:nth-child(1)"
      span2nd <- shouldSelect "span:nth-child(2)"
      pure $ para1st >>= const span2nd
    it "creates an element on a specified parent" $ do
      let divId = "putithere"
      body <- documentBody
      render body Nothing $ Just $ node "div" [] [("id", divId)] []
      parentDiv <- getElementById divId
      render parentDiv Nothing $ Just $ node "p" [] [] []
      shouldSelect "div > p"

attributesSpec : SpecTree' FFI_JS
attributesSpec =
  describe "attributes" $ do
    it "sets a single attribute on an element" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [("class", "testval")] []
      shouldSelect "p.testval"
    it "overwrites attributes with duplicate keys" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [ ("class", "badval")
                            , ("class", "goodval")
                            ] []
      noBadval <- shouldNotSelect "p.badval"
      hasGoodval <- shouldSelect "p.goodval"
      pure $ noBadval >>= const hasGoodval
    it "sets multiple attributes on an element" $ do
      body <- documentBody
      render body Nothing $ Just $ node "p" [] [ ("class", "classval")
                            , ("title", "titleval")
                            ] []
      shouldSelect "p.classval[title=titleval]"
    it "sets different attributes on multiple elements" $ do
      body <- documentBody
      render body Nothing $ Just $ node "div" [] [] [ node "p" [] [("class", "a")] []
                                 , node "p" [] [("class", "b")] []
                                 ]
      a <- shouldSelect "p.a"
      b <- shouldSelect "p.b"
      ab <- shouldNotSelect "p.a.b"
      pure $ a >>= const b >>= const ab

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
      pre <- shouldNotSelect "p"
      dispatchEventOnId "click" "doit"
      post <- shouldSelect "p"
      pure $ pre >>= const post
    it "respects the once option on listeners" $ do
      let opts = record { once = Just True } noOptions
      body <- documentBody
      render body Nothing $ Just $ node "button"
        [on "click" (const appendBodyParagraph) opts] [("id", "doit")] []
      dispatchEventOnId "click" "doit"
      dispatchEventOnId "click" "doit"
      
      p1 <- shouldSelect "p:only-of-type"
      p2 <- shouldNotSelect "p:nth-of-type(2)"
      pure $ p1 >>= const p2
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
      p1 <- shouldSelect "p#first:nth-of-type(1)"
      p2 <- shouldSelect "p#second:nth-of-type(2)"
      pure $ p1 >>= const p2
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
      p1 <- shouldSelect "p#first:nth-of-type(1)"
      p2 <- shouldSelect "p#second:nth-of-type(2)"
      pure $ p1 >>= const p2

elementsSpecSubsequent : SpecTree' FFI_JS
elementsSpecSubsequent =
  describe "elements" $ do
    it "leaves empty DOM empty when old and new virtual DOMs are Nothing" $ do
      body <- documentBody
      pre <- shouldNotSelect "body *"
      render body Nothing Nothing
      post <- shouldNotSelect "body *"
      pure $ pre >>= const post
    it "empties DOM when old virtual DOM is Just and new one is Nothing" $ do
      body <- documentBody
      let html = node "div" [] [] [ node "p" [] [] [] ]
      render body Nothing (Just html)
      pre <- shouldSelect "body > div > p"
      render body (Just html) Nothing
      post <- shouldNotSelect "body *"
      pure $ pre >>= const post
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
        noDiv <- shouldNotSelect "body > div"
        twoParas <- shouldSelect "body > p > div:nth-of-type(2)"
        pure $ noDiv >>= const twoParas

attributesSpecSubsequent : SpecTree' FFI_JS
attributesSpecSubsequent =
  describe "attributes" $ do
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
        pre <- shouldSelect "body > div[style][title]"
        render body oldHtml newHtml
        noStyle <- shouldNotSelect "body > div[style][title]"
        hasTitle <- shouldSelect "body > div[title]"
        pure $ pre >>= const noStyle >>= const hasTitle
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
        pre1 <- shouldNotSelect "body > div[style][title]"
        pre2 <- shouldSelect "body > div[title]"
        render body oldHtml newHtml
        post <- shouldSelect "body > div[style][title]"
        pure $ pre1 >>= const pre2 >>= const post

eventsSpecSubsequent : SpecTree' FFI_JS
eventsSpecSubsequent =
  describe "events" $ do
    it "does not execute removed event handlers" $ do
      let makeButton = \handlers =>
        Just $ node "button" handlers [("id", "doit")] []
      let html1 = makeButton [on "click" (const appendBodyParagraph) noOptions]
      let html2 = makeButton []
      body <- documentBody
      render body Nothing html1
      render body html1 html2
      dispatchEventOnId "click" "doit"
      shouldNotSelect "p"
    it "executes replacement event handlers" $ do
      let makeButton = \handlers =>
        Just $ node "button" handlers [("id", "doit")] []
      let makeHandler = \paraId =>
        on "click" (const $ appendBodyParagraphWithId paraId) noOptions
      let html1 = makeButton [makeHandler "old"]
      let html2 = makeButton [makeHandler "new"]
      body <- documentBody
      render body Nothing html1
      render body html1 html2
      dispatchEventOnId "click" "doit"
      shouldSelect "p#new"
    it "leaves descendants intact when replacing handlers" $
      let
        makeHtml = \event => Just $
          node "div" [ on event (const $ pure ()) noOptions ] []
            [ node "p" [] []
              [ node "button" [] [("id","thebutton")] [] ]
            , node "span" [] [("id","thespan")] []
            ]
        html1 = makeHtml "mousedown"
        html2 = makeHtml "mouseup"
      in do
        body <- documentBody
        render body Nothing html1
        render body html1 html2
        btnExists <- shouldSelect "div>p>button#thebutton"
        spanExists <- shouldSelect "div>span:nth-child(2)#thespan"
        pure $ btnExists >>= const spanExists

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
    attributesSpec
    eventsSpec
  describe "subsequent rendering" $ do
    elementsSpecSubsequent
    attributesSpecSubsequent
    eventsSpecSubsequent