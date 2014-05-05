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

  onreadystatechange :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  onreadystatechange = on "onreadystatechange"
  onabort            :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  onabort            = on "onabort"
  onerror            :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  onerror            = on "onerror"
  onload             :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  onload             = on "onload"
  onloadend          :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  onloadend          = on "onloadend"
  onloadstart        :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  onloadstart        = on "onloadstart"
  onprogress         :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  onprogress         = on "onprogress"
  ontimeout          :: Ajax -> (XHRAjax -> EffAjax) -> EffAjax
  ontimeout          = on "ontimeout"

  currentTarget :: Ajax -> XHRAjax
  currentTarget = attr "currentTarget"
  response      :: Ajax -> Response
  response      = attr "response"
  responseText  :: Ajax -> String
  responseText  = attr "responseText"
  responseXML   :: Ajax -> String
  responseXML   = attr "responseXML"
  status        :: Ajax -> StatusCode
  status        = attr "status"
  statusText    :: Ajax -> String
  statusText    = attr "statusText"

  readyState :: Ajax -> ReadyState
  readyState x = case attr "readyState" x of
    0 -> Unsent
    1 -> Opened
    2 -> HeadersReceived
    3 -> Loading
    4 -> Done

  getResponseHeader :: Ajax -> HeaderHead -> String
  getResponseHeader x = attr "getResponseHeader" x <<< show

  setRequestHeader :: Ajax -> Header -> XHRAjax
  setRequestHeader x (Header header value) =
    curry2 (attr "setRequestHeader" x) (show header) value

  ajax2XHR :: Ajax -> XHR
  ajax2XHR = either getXhr getXhr
    where
      getXhr :: forall r. {xhr :: XHR | r} -> XHR
      getXhr o = o.xhr

  open :: Request -> XHRAjax
  open req@{xhr = x, verb = v, uri = u} =
    pure <<< Left $ req {xhr = unsafeOpen x (show v) u}

  -- send :: Request -> XHRAjax
  -- send req@{xhr = x} =
  --   let xhr' = unsafeSend x
  --   in pure <<< Right $ {status = status xhr', xhr = unsafeSend x

  -- | FFI Calls

  foreign import attr
    "function attr(a) {\
    \  return function(x) {\
    \    return x[a];\
    \  }\
    \}" :: forall a. String -> Ajax -> a

  foreign import on
    "function on(method) {\
    \  return function(x) {\
    \    return function(f) {\
    \      return function() {\
    \        x[method] = f;\
    \        return x;\
    \      }\
    \    }\
    \  }\
    \}" :: String -> Ajax -> (XHRAjax -> EffAjax) -> EffAjax

  foreign import unsafeOpen
    "function unsafeOpen(x) {\
    \  return function(verb) {\
    \    return function(uri) {\
    \      x.open(verb, uri);\
    \      return x;\
    \    }\
    \  }\
    \}" :: XHR -> String -> URI -> XHR

  foreign import unsafeSend
    "function unsafeSend(x) {\
    \  x.xhr.send();\
    \  return x;\
    \}" :: XHR -> XHR

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
