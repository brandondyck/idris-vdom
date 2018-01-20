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

log : a -> JS_IO ()
log x = jscall "console.log(%0)" (Ptr -> JS_IO ()) (believe_me x)

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

singleElementIsCreated : JS_IO ()
singleElementIsCreated = do
  body <- clearBody
  createElement "p" >>= appendChild body

  True <- selectorExists "p"
    | log "Fail: no <p> element"
  log "Pass"

nestedElementsAreCreated : JS_IO ()
nestedElementsAreCreated = do
  body <- clearBody
  pElement <- createElement "p" >>= appendChild body
  True <- selectorExists "p"
    | log "Fail: no <p> element"

  createElement "span" >>= appendChild pElement
  True <- selectorExists "span"
    | log "Fail: no <span> element"
  log "Pass"

main : JS_IO ()
main = do
  singleElementIsCreated
  nestedElementsAreCreated
