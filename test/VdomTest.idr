module Main

import VirtualDOM
import VirtualDOM.DOM

{-
Things to test:

☐ Single element is created
☐ Nested elements are created
☐ Events work on elements
☐ Properties are created on element
☐ Node is created on different root than body
-}

log : a -> JS_IO ()
log x = jscall "console.log(%0)" (Ptr -> JS_IO ()) (believe_me x)

singleElementIsCreated : JS_IO ()
singleElementIsCreated = do
  -- Setup
  body <- documentBody
  setInnerHTML body ""
  createElement "p" >>= appendChild body

  -- Test
  pElement <- jscall "document.querySelector('p')" (JS_IO Ptr)
  isNull <- jscall "(%0 === null) + ''" (Ptr -> JS_IO String) pElement
  log isNull
  if isNull == "true"
    -- Fail
    then log "Fail: no <p> element"
    else log "Pass"

main : JS_IO ()
main = do
  singleElementIsCreated
