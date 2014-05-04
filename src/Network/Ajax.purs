module Network.Ajax where

  import Control.Monad.Eff

  foreign import data XHR :: !

  type URI = String

  data Verb = DELETE
            | GET
            | HEAD
            | OPTIONS
            | PATCH
            | POST
            | PUT

  data Header = Accept          String
              | ContentEncoding String
              | ContentLength   String
              | ContentLocation String
              | ContentMD5      String
              | ContentRange    String
              | ContentType     String
              | Expect          String
              | Host            String
              | UserAgent       String

  data ReadyState = Done
                  | HeadersReceived
                  | Loading
                  | Opened
                  | Unsent

  data StatusCode = Accepted
                  | BadGateway
                  | BadRequest
                  | Continue
                  | Created
                  | ExpectationFailed
                  | Forbidden
                  | Found
                  | GatewayTimeout
                  | Gone
                  | HTTPVersionNotSupported
                  | InternalServerError
                  | LengthRequired
                  | MethodNotAllowed
                  | MovedPermanently
                  | MultipleChoices
                  | NoContent
                  | NonAuthoritativeInformation
                  | NotAcceptable
                  | NotFound
                  | NotImplemented
                  | NotModified
                  | Ok
                  | PartialContent
                  | PaymentRequired
                  | PreconditionFailed
                  | ProxyAuthenticationRequired
                  | RequestedRangeNotSatisfiable
                  | RequestEntityTooLarge
                  | RequestTimeout
                  | RequestURITooLong
                  | ResetContent
                  | SeeOther
                  | ServiceUnavailable
                  | SwitchingProtocols
                  | TemporaryRedirect
                  | Unauthorized
                  | UnsupportedMediaType
                  | UseProxy

  instance showHTTPVerb :: Show Verb where
    show DELETE  = "DELETE"
    show GET     = "GET"
    show HEAD    = "HEAD"
    show OPTIONS = "OPTIONS"
    show PATCH   = "PATCH"
    show POST    = "POST"
    show PUT     = "PUT"

  instance eqReadyState :: Eq ReadyState where
    (==) Done            Done            = true
    (==) HeadersReceived HeadersReceived = true
    (==) Loading         Loading         = true
    (==) Opened          Opened          = true
    (==) Unsent          Unsent          = true
    (==) _               _               = false

    (/=) rs              rs'             = not (rs == rs')

  header2Head :: Header -> String
  header2Head (Accept _)          = "Accept"
  header2Head (ContentEncoding _) = "Content-Encoding"
  header2Head (ContentLength _)   = "Content-Length"
  header2Head (ContentLocation _) = "Content-Location"
  header2Head (ContentMD5 _)      = "Content-MD5"
  header2Head (ContentRange _)    = "Content-Range"
  header2Head (ContentType _)     = "Content-Type"
  header2Head (Expect _)          = "Expect"
  header2Head (Host _)            = "Host"
  header2Head (UserAgent _)       = "User-Agent"

  header2Value :: Header -> String
  header2Value (Accept v)          = v
  header2Value (ContentEncoding v) = v
  header2Value (ContentLength v)   = v
  header2Value (ContentLocation v) = v
  header2Value (ContentMD5 v)      = v
  header2Value (ContentRange v)    = v
  header2Value (ContentType v)     = v
  header2Value (Expect v)          = v
  header2Value (Host v)            = v
  header2Value (UserAgent v)       = v

  status2Number :: StatusCode -> Number
  -- 100 Informational
  status2Number Continue                     = 100
  status2Number SwitchingProtocols           = 101
  -- 200 Successful
  status2Number Ok                           = 200
  status2Number Created                      = 201
  status2Number Accepted                     = 202
  status2Number NonAuthoritativeInformation  = 203
  status2Number NoContent                    = 204
  status2Number ResetContent                 = 205
  status2Number PartialContent               = 206
  -- 300 Redirection
  status2Number MultipleChoices              = 300
  status2Number MovedPermanently             = 301
  status2Number Found                        = 302
  status2Number SeeOther                     = 303
  status2Number NotModified                  = 304
  status2Number UseProxy                     = 305
  status2Number TemporaryRedirect            = 307
  -- 400 Client Error
  status2Number BadRequest                   = 400
  status2Number Unauthorized                 = 401
  status2Number PaymentRequired              = 402
  status2Number Forbidden                    = 403
  status2Number NotFound                     = 404
  status2Number MethodNotAllowed             = 405
  status2Number NotAcceptable                = 406
  status2Number ProxyAuthenticationRequired  = 407
  status2Number RequestTimeout               = 408
  status2Number Gone                         = 410
  status2Number LengthRequired               = 411
  status2Number PreconditionFailed           = 412
  status2Number RequestEntityTooLarge        = 413
  status2Number RequestURITooLong            = 414
  status2Number UnsupportedMediaType         = 415
  status2Number RequestedRangeNotSatisfiable = 416
  status2Number ExpectationFailed            = 417
  -- 500 Server Error
  status2Number InternalServerError          = 500
  status2Number NotImplemented               = 501
  status2Number BadGateway                   = 502
  status2Number ServiceUnavailable           = 503
  status2Number GatewayTimeout               = 504
  status2Number HTTPVersionNotSupported      = 505

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
