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
    -- This is obviously missing stuff, maybe this isn't actually an `Apply`?

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
  response      :: forall a. XHRAjax -> Ajax a
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

  -- | FFI Calls

  foreign import attr
    "function attr(a) {\
    \  return function(x) {\
    \    return x.currentTarget;\
    \  }\
    \}" :: forall a. String -> XHRAjax -> a

  foreign import on
    "function on(method) {\
    \  return function(x) {\
    \    return function(f) {\
    \      x[method] = f;\
    \      return function() { return x; }\
    \    }\
    \  }\
    \}" :: String -> XHRAjax -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import open
    "function open(x) {\
    \  return function(verb) {\
    \    return function(uri) {\
    \      var verbStr = PS.Prelude.show(PS.Network_HTTP.showHTTPVerb())(verb);\
    \      x.open(verbStr, uri);\
    \      return function() { return x; }\
    \    }\
    \  }\
    \}" :: XHRAjax-> Verb -> URI -> XHRAjax

  foreign import send
    "function send(x) {\
    \  x.send();\
    \  return function() { return x; }\
    \}" :: XHRAjax -> XHRAjax

  foreign import xhr
    "function xhr() {\
    \  return new XMLHttpRequest();\
    \}" :: XHRAjax

  foreign import curry2
    "function curry2(f) {\
    \  return function(a) {\
    \    return function(b) {\
    \      return f(a, b);\
    \    }\
    \  }\
    \}" :: forall a b c. (a -> b -> c) -> a -> b -> c
