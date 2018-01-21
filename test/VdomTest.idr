module Main

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

data TestResult = Pass | Fail String

failureMsg : TestResult -> Maybe String
failureMsg Pass = Nothing
failureMsg (Fail msg) = Just msg

clearBody : JS_IO Node
clearBody = do
  body <- documentBody
  setInnerHTML body ""
  pure body

querySelector : String -> JS_IO Ptr
querySelector = jscall "document.querySelector(%0)" _

isNull : Ptr -> JS_IO Bool
isNull ptr = (== "true") <$>
  jscall "(%0 === null) + ''" (Ptr -> JS_IO String) ptr

selectorExists : String -> JS_IO Bool
selectorExists selector = not <$> (querySelector selector >>= isNull)

singleElementIsCreated : JS_IO TestResult
singleElementIsCreated = do
  body <- clearBody
  createElement "p" >>= appendChild body

  True <- selectorExists "p"
    | False => pure (Fail "no <p> element")
  pure Pass

nestedElementsAreCreated : JS_IO TestResult
nestedElementsAreCreated = do
  body <- clearBody
  pElement <- createElement "p" >>= appendChild body
  True <- selectorExists "p"
    | False => pure (Fail "no <p> element")

  createElement "span" >>= appendChild pElement
  True <- selectorExists "span"
    | False => pure (Fail "no <span> element")
  pure Pass

main : JS_IO ()
main = do
  let tests = [ singleElementIsCreated
              , nestedElementsAreCreated
              ]
  msgs <- mapMaybe failureMsg <$> sequence tests
  case msgs of
       [] => putStrLn' "PASS"
       _ => traverse_ (putStrLn' . ("FAIL: " ++)) msgs
  
  
