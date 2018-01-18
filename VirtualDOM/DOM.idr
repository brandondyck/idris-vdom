module VirtualDOM.DOM

%default total
%access export

export
record Node where
  constructor MkNode
  unNode : Ptr

%inline
jscall : (name : String) -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} ->ty
jscall name ty = foreign FFI_JS name ty

documentBody : JS_IO Node
documentBody = MkNode <$> jscall "document.body" _

appendChild : (parent : Node) -> (child : Node) -> JS_IO Node
appendChild parent child = MkNode <$>
  jscall "(%0).appendChild(%1)" (Ptr -> Ptr -> JS_IO Ptr)
  (unNode parent) (unNode child)

setAttribute : Node -> (name : String) -> (value : String) -> JS_IO ()
setAttribute = jscall "%0.setAttribute(%1, %2)"
  (Ptr -> String -> String -> JS_IO ()) . unNode

createElement : (tag : String) -> JS_IO Node
createElement = liftA MkNode . jscall "document.createNode(%0)" _

createTextNode : (text : String) -> JS_IO Node
createTextNode = liftA MkNode . jscall "document.createTextNode(%0)" _

setInnerHTML : (element : Node) -> (html : String) -> JS_IO ()
setInnerHTML = jscall "%0.innerHTML = %1" (Ptr -> String -> JS_IO ()) . unNode

partial
addEventListener : (eventTarget : Node) -> (eventName : String) ->
                  (listener : Ptr -> JS_IO ()) -> JS_IO ()
addEventListener eventTarget eventName listener =
  jscall "%0.addEventListener(%1, %2)"
    (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> JS_IO ())
    (unNode eventTarget) eventName (MkJsFn listener)
