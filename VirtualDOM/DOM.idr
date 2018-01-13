module VirtualDOM.DOM

%default total
%access export

%inline
jscall : (name : String) -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} ->ty
jscall name ty = foreign FFI_JS name ty

documentBody : JS_IO Ptr
documentBody = jscall "document.body" _

appendChild : Ptr -> Ptr -> JS_IO Ptr
appendChild = jscall "(%0).appendChild(%1)" _

setAttribute : Ptr -> String -> String -> JS_IO ()
setAttribute = jscall "%0.setAttribute(%1, %2)" _

createElement : (tag : String) -> JS_IO Ptr
createElement = jscall "document.createElement(%0)" _

createTextNode : (text : String) -> JS_IO Ptr
createTextNode = jscall "document.createTextNode(%0)" _

setInnerHTML : Ptr -> String -> JS_IO ()
setInnerHTML = jscall "%0.innerHTML = %1" _

partial
addEventListener : (eventTarget : Ptr) -> (eventName : String) ->
                  (listener : Ptr -> JS_IO ()) -> JS_IO ()
addEventListener eventTarget eventName listener =
  jscall "%0.addEventListener(%1, %2)"
    (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO ())
    eventTarget eventName (MkJsFn listener)
