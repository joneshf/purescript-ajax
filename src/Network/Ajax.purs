module Network.Ajax where

  import Control.Monad.Eff

  import qualified Data.Array as A
  import Data.Either

  import Network.HTTP

  foreign import data XHR :: *

  foreign import data XHREff :: !

  -- Clean up signatures.
  type XHRAjax = forall a eff. Eff (x :: XHREff | eff) Ajax
  type EffAjax = forall a eff. Eff eff Ajax

  type URI = String
  -- This should be something like data Payload = JSON | XML | PlainText | ...
  type Payload = String

  type Ajax = Either Request Response
  type Request = {verb :: Verb, uri :: URI, headers :: [Header], content :: Payload, xhr :: XHR}
  type Response = {status :: StatusCode, headers :: [Header], content :: Payload, xhr :: XHR}

  data ReadyState = Done
                  | HeadersReceived
                  | Loading
                  | Opened
                  | Unsent

  instance eqReadyState :: Eq ReadyState where
    (==) Done            Done            = true
    (==) HeadersReceived HeadersReceived = true
    (==) Loading         Loading         = true
    (==) Opened          Opened          = true
    (==) Unsent          Unsent          = true
    (==) _               _               = false

    (/=) rs              rs'             = not (rs == rs')

  delete  :: URI -> XHRAjax
  delete  = httpVerb DELETE
  get     :: URI -> XHRAjax
  get     = httpVerb GET
  head    :: URI -> XHRAjax
  head    = httpVerb HEAD
  options :: URI -> XHRAjax
  options = httpVerb OPTIONS
  patch   :: URI -> XHRAjax
  patch   = httpVerb PATCH
  post    :: URI -> XHRAjax
  post    = httpVerb POST
  put     :: URI -> XHRAjax
  put     = httpVerb PUT

  httpVerb :: Verb -> URI -> XHRAjax
  httpVerb v u = pure $ Left {verb: v, uri: u, headers: [], content: "", xhr: newXhr}

  onreadystatechange :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  onreadystatechange = on "onreadystatechange"
  onabort            :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  onabort            = on "onabort"
  onerror            :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  onerror            = on "onerror"
  onload             :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  onload             = on "onload"
  onloadend          :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  onloadend          = on "onloadend"
  onloadstart        :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  onloadstart        = on "onloadstart"
  onprogress         :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  onprogress         = on "onprogress"
  ontimeout          :: XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax
  ontimeout          = on "ontimeout"

  currentTarget :: XHRAjax -> XHRAjax
  currentTarget = attr "currentTarget"
  response      :: XHRAjax -> Response
  response      = attr "response"
  responseText  :: XHRAjax -> String
  responseText  = attr "responseText"
  responseXML   :: XHRAjax -> String
  responseXML   = attr "responseXML"
  status        :: XHRAjax -> StatusCode
  status        = attr "status"
  statusText    :: XHRAjax -> String
  statusText    = attr "statusText"

  readyState :: XHRAjax -> ReadyState
  readyState x = case attr "readyState" x of
    0 -> Unsent
    1 -> Opened
    2 -> HeadersReceived
    3 -> Loading
    4 -> Done

  getResponseHeader :: XHRAjax -> HeaderHead -> String
  getResponseHeader x = attr "getResponseHeader" x <<< show

  setRequestHeader :: XHRAjax -> Header -> XHRAjax
  setRequestHeader x (Header header value) =
    curry2 (attr "setRequestHeader" x) (show header) value

  ajax2XHR :: Ajax -> XHR
  ajax2XHR = either getXhr getXhr
    where
      getXhr :: forall r. {xhr :: XHR | r} -> XHR
      getXhr o = o.xhr

  -- | FFI Calls

  foreign import attr
    "function attr(a) {\
    \  return function(x) {\
    \    return ajax2XHR(x).currentTarget;\
    \  }\
    \}" :: forall a. String -> XHRAjax -> a

  foreign import on
    "function on(method) {\
    \  return function(x) {\
    \    return function(f) {\
    \      ajax2XHR(x)[method] = f;\
    \      return function() { return x; }\
    \    }\
    \  }\
    \}" :: String -> XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import open
    "function open(x) {\
    \  return function(verb) {\
    \    return function(uri) {\
    \      var verbStr = PS.Prelude.show(PS.Network_HTTP.showHTTPVerb())(verb);\
    \      ajax2XHR(x).open(verbStr, uri);\
    \      return function() { return x; }\
    \    }\
    \  }\
    \}" :: XHRAjax-> Verb -> URI -> XHRAjax

  foreign import send
    "function send(x) {\
    \  ajax2XHR(x).send();\
    \  return function() { return x; }\
    \}" :: XHRAjax -> XHRAjax

  foreign import newXhr
    "var newXhr = new XMLHttpRequest();" :: XHR

  foreign import curry2
    "function curry2(f) {\
    \  return function(a) {\
    \    return function(b) {\
    \      return f(a, b);\
    \    }\
    \  }\
    \}" :: forall a b c. (a -> b -> c) -> a -> b -> c
