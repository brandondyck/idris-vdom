module VirtualDOM.DOM

import Data.List

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

getElementById : (elementId : String) -> JS_IO Node
getElementById elementId = MkNode <$>
  jscall "document.getElementById(%0)"
    (String -> JS_IO Ptr) elementId

nthChild : (parent : Node) -> (n : Int) -> JS_IO Node
nthChild parent n = MkNode <$>
  jscall "%0.childNodes[%1]" (Ptr -> Int -> JS_IO Ptr) (unNode parent) n

appendChild : (parent : Node) -> (child : Node) -> JS_IO Node
appendChild parent child = MkNode <$>
  jscall "%0.appendChild(%1)" (Ptr -> Ptr -> JS_IO Ptr)
  (unNode parent) (unNode child)

removeChild : (parent : Node) -> (child : Node) -> JS_IO Node
removeChild parent child = MkNode <$>
  jscall "%0.removeChild(%1)" (Ptr -> Ptr -> JS_IO Ptr)
  (unNode parent) (unNode child)

replaceChild : (parent : Node) -> (newChild : Node) -> (oldChild : Node) ->
               JS_IO ()
replaceChild parent newChild oldChild =
  jscall "%0.replaceChild(%1, %2)" (Ptr -> Ptr -> Ptr -> JS_IO ())
  (unNode parent) (unNode newChild) (unNode oldChild)

setAttribute : Node -> (name : String) -> (value : String) -> JS_IO ()
setAttribute = jscall "%0.setAttribute(%1, %2)"
  (Ptr -> String -> String -> JS_IO ()) . unNode

removeAttribute : Node -> (name : String) -> JS_IO ()
removeAttribute = jscall "%0.removeAttribute(%1)"
  (Ptr -> String -> JS_IO ()) . unNode

createElement : (tag : String) -> JS_IO Node
createElement = liftA MkNode . jscall "document.createElement(%0)" _

createTextNode : (text : String) -> JS_IO Node
createTextNode = liftA MkNode . jscall "document.createTextNode(%0)" _

setInnerHTML : (element : Node) -> (html : String) -> JS_IO ()
setInnerHTML = jscall "%0.innerHTML = %1" (Ptr -> String -> JS_IO ()) . unNode

record ListenerOptions where
  constructor MkListenerOptions
  once : Maybe Bool
  capture : Maybe Bool
  passive : Maybe Bool

noOptions : ListenerOptions
noOptions = MkListenerOptions Nothing Nothing Nothing

jsonParse : (json : String) -> JS_IO Ptr
jsonParse = jscall "JSON.parse(%0)" _

optionsToPtr : ListenerOptions -> JS_IO Ptr
optionsToPtr options =
  let
    optStrings =
      mapMaybe optToJsonField
        [ ("once", once)
        , ("capture", capture)
        , ("passive", passive)
        ]
    optString = "{" ++ (foldr (++) "" (intersperse "," optStrings)) ++ "}"
  in
    jsonParse optString
  where
    optToJsonField : (String, ListenerOptions -> Maybe Bool) -> Maybe String
    optToJsonField (name, getVal) = do
      val <- getVal options
      let valJson = if val then "true" else "false"
      pure ("\"" ++ name ++ "\"" ++ ":" ++ valJson)

partial
addEventListener : (eventTarget : Node) -> (eventName : String) ->
                   (listener : Ptr -> JS_IO ()) ->
                   (options : ListenerOptions) ->
                   JS_IO ()
addEventListener eventTarget eventName listener options =
  optionsToPtr options >>=
    jscall "%0.addEventListener(%1, %2, %3)"
      (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> Ptr -> JS_IO ())
      (unNode eventTarget) eventName (MkJsFn listener)

partial
removeEventListener : (eventTarget : Node) -> (eventName : String) ->
                      (listener : Ptr -> JS_IO ()) ->
                      (options : ListenerOptions) ->
                      JS_IO ()
removeEventListener eventTarget eventName listener options =
  optionsToPtr options >>=
    jscall "%0.removeEventListener(%1, %2, %3)"
      (Ptr -> String -> JsFn (Ptr -> JS_IO ()) -> Ptr -> JS_IO ())
      (unNode eventTarget) eventName (MkJsFn listener)

dispatchEvent : (eventName : String) -> (bubbles : Bool) -> (eventTarget : Node) -> JS_IO Bool
dispatchEvent eventName bubbles eventTarget = do
  let bubblesInt = if bubbles then 1 else 0
  sbool <- jscall "%0.dispatchEvent(new Event(%1,{bubbles:%2===1})).toString()"
    (Ptr -> String -> Int -> JS_IO String)
    (unNode eventTarget) eventName bubblesInt
  pure $ sbool == "true"
