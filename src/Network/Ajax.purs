module Network.Ajax where

  import Control.Monad.Eff

  import Network.HTTP

  foreign import data XHR :: !

  type URI = String

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

  delete :: forall eff a. URI -> Eff (x :: XHR | eff) a
  delete = httpVerb DELETE

  get :: forall eff a. URI -> Eff (x :: XHR | eff) a
  get = httpVerb GET

  head :: forall eff a. URI -> Eff (x :: XHR | eff) a
  head = httpVerb HEAD

  options :: forall eff a. URI -> Eff (x :: XHR | eff) a
  options = httpVerb OPTIONS

  patch :: forall eff a. URI -> Eff (x :: XHR | eff) a
  patch = httpVerb PATCH

  post :: forall eff a. URI -> Eff (x :: XHR | eff) a
  post = httpVerb POST

  put :: forall eff a. URI -> Eff (x :: XHR | eff) a
  put = httpVerb PUT

  httpVerb :: forall eff a. Verb -> URI -> Eff (x :: XHR | eff) a
  httpVerb = open xhr

  -- | FFI Calls

  foreign import currentTarget
    "function currentTarget(x) {\
    \  return x.currentTarget;\
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> Eff (x :: XHR | eff) a

  foreign import getResponseHeader
    "function getResponseHeader(x) {\
    \  return function(header) {\
    \    console.log(x.getResponseHeader(header2Head(header)));\
    \  }\
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> Header -> Eff eff a

  foreign import onreadystatechange
    "function onreadystatechange(x) {\
    \  return function(f) {\
    \    x.onreadystatechange = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  -- These might need to be a different type.
  foreign import onabort
    "function onabort(x) {\
    \  return function(f) {\
    \    x.onabort = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  foreign import onerror
    "function onerror(x) {\
    \  return function(f) {\
    \    x.onerror = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  foreign import onload
    "function onload(x) {\
    \  return function(f) {\
    \    x.onload = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  foreign import onloadend
    "function onloadend(x) {\
    \  return function(f) {\
    \    x.onloadend = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  foreign import onloadstart
    "function onloadstart(x) {\
    \  return function(f) {\
    \    x.onloadstart = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  foreign import onprogress
    "function onprogress(x) {\
    \  return function(f) {\
    \    x.onprogress = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  foreign import ontimeout
    "function ontimeout(x) {\
    \  return function(f) {\
    \    x.ontimeout = f;\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a x. Eff (x :: XHR | eff) a -> (Eff (x :: XHR | eff) a -> Eff eff a) -> Eff eff a

  foreign import open
    "function open(x) {\
    \  return function(verb) {\
    \    return function(uri) {\
    \      var verbStr = PS.Prelude.show(showHTTPVerb())(verb);\
    \      x.open(verbStr, uri);\
    \      return function() { return x; }\
    \    }\
    \  }\
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> Verb -> URI -> Eff (x :: XHR | eff) a

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
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> ReadyState

  foreign import responseText
    "function responseText(x) {\
    \  console.log(x.responseText);\
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> Eff eff a

  foreign import responseXML
    "function responseXML(x) {\
    \  console.log(x.responseXML);\
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> Eff eff a

  foreign import send
    "function send(x) {\
    \  x.send();\
    \  return function() { return x; }\
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> Eff (x :: XHR | eff) a

  foreign import setRequestHeader
    "function setRequestHeader(x) {\
    \  return function(header) {\
    \    x.setRequestHeader(header2Head(header), header2Value(header));\
    \    return function() { return x; }\
    \  }\
    \}" :: forall eff a. Eff (x :: XHR | eff) a -> Header -> Eff (x :: XHR | eff) a

  foreign import xhr
    "var xhr = new XMLHttpRequest();" :: forall eff a. Eff (x :: XHR | eff) a
