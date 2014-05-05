module Network.Ajax where

  import Control.Monad.Eff

  import qualified Data.Array as A

  import Network.HTTP

  foreign import data XHR :: !

  -- Clean up signatures.
  type XHRAjax = forall a eff. Eff (x :: XHR | eff) (Ajax a)
  type EffAjax = forall a eff. Eff eff (Ajax a)

  type URI = String

  -- This should be something like data Payload = JSON | XML | PlainText | ...
  type Payload = String

  data Ajax a = GetRequest URI [Header]
              | PostRequest URI [Header] Payload
              | Response [Header] a

  instance functorAjax :: Functor Ajax where
    (<$>) f (Response h x) = Response h (f x)
    (<$>) _ (GetRequest  u h)   = GetRequest  u h
    (<$>) _ (PostRequest u h p) = PostRequest u h p

  instance applyAjax :: Apply Ajax where
    (<*>) (Response h f) (Response h' x) = Response (h ++ h') (f x)

  instance bindAjax :: Bind Ajax where
    (>>=) (Response h x) f = f x
    (>>=) (GetRequest  u h)   _ = GetRequest  u h
    (>>=) (PostRequest u h p) _ = PostRequest u h p
    -- Should this instead dish off the request and work with the response?
    -- (>>=) (GetRequest  u h) f = response (get  u >>= \x -> send x) >>= f
    -- (>>=) (PostRequest u h) f = response (post u >>= \x -> send x) >>= f

  instance applicativeAjax :: Applicative Ajax where
    pure x = Response [] x

  instance monadAjax :: Monad Ajax

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

  delete :: URI -> XHRAjax
  delete = httpVerb DELETE

  get :: URI -> XHRAjax
  get = httpVerb GET

  head :: URI -> XHRAjax
  head = httpVerb HEAD

  options :: URI -> XHRAjax
  options = httpVerb OPTIONS

  patch :: URI -> XHRAjax
  patch = httpVerb PATCH

  post :: URI -> XHRAjax
  post = httpVerb POST

  put :: URI -> XHRAjax
  put = httpVerb PUT

  httpVerb :: Verb -> URI -> XHRAjax
  httpVerb v u = xhr >>= \x -> open x v u

  -- | FFI Calls

  foreign import currentTarget
    "function currentTarget(x) {\
    \  return x.currentTarget;\
    \}" :: forall a. Ajax a -> XHRAjax

  foreign import getResponseHeader
    "function getResponseHeader(x) {\
    \  return function(header) {\
    \    console.log(x.getResponseHeader(header2Head(header)));\
    \  }\
    \}" :: forall a. Ajax a -> Header -> EffAjax

  foreign import onreadystatechange
    "function onreadystatechange(x) {\
    \  return function(f) {\
    \    x.onreadystatechange = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  -- These might need to be a different type.
  foreign import onabort
    "function onabort(x) {\
    \  return function(f) {\
    \    x.onabort = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import onerror
    "function onerror(x) {\
    \  return function(f) {\
    \    x.onerror = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import onload
    "function onload(x) {\
    \  return function(f) {\
    \    x.onload = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import onloadend
    "function onloadend(x) {\
    \  return function(f) {\
    \    x.onloadend = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import onloadstart
    "function onloadstart(x) {\
    \  return function(f) {\
    \    x.onloadstart = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import onprogress
    "function onprogress(x) {\
    \  return function(f) {\
    \    x.onprogress = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import ontimeout
    "function ontimeout(x) {\
    \  return function(f) {\
    \    x.ontimeout = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall a. Ajax a -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import open
    "function open(x) {\
    \  return function(verb) {\
    \    return function(uri) {\
    \      var verbStr = PS.Prelude.show(PS.Network_HTTP.showHTTPVerb())(verb);\
    \      x.open(verbStr, uri);\
    \      return function() { return x; }\
    \    }\
    \  }\
    \}" :: forall a. Ajax a -> Verb -> URI -> XHRAjax

  foreign import readyState
    "function readyState(x) {\
    \  switch (x.readyState) {\
    \    case 0: return Unsent;\
    \    case 1: return Opened;\
    \    case 2: return HeadersReceived;\
    \    case 3: return Loading;\
    \    case 4: return Done;\
    \    default: throw 'Failed pattern match';\
    \  }\
    \}" :: forall a. Ajax a -> ReadyState

  foreign import response
    "function response(x) {\
    \  return x.response;\
    \}" :: forall a. XHRAjax -> Ajax a

  foreign import responseText
    "function responseText(x) {\
    \  return x.responseText;\
    \}" :: XHRAjax -> String

  foreign import responseXML
    "function responseXML(x) {\
    \  return x.responseXML;\
    \}" :: XHRAjax -> String

  foreign import send
    "function send(x) {\
    \  x.send();\
    \  return function() { return x; }\
    \}" :: XHRAjax -> XHRAjax

  foreign import setRequestHeader
    "function setRequestHeader(x) {\
    \  return function(header) {\
    \    x.setRequestHeader(header2Head(header), header2Value(header));\
    \    return function() { return x; }\
    \  }\
    \}" :: XHRAjax -> Header -> XHRAjax

  foreign import status
    "function status(x) {\
    \  return x.status;\
    \}" :: XHRAjax -> StatusCode

  foreign import statusText
    "function statusText(x) {\
    \  return x.statusText;\
    \}" :: XHRAjax -> String

  foreign import xhr
    "function xhr() {\
    \  return new XMLHttpRequest();\
    \}" :: XHRAjax
