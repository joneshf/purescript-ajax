module Network.HTTP where

  data Verb = DELETE
            | GET
            | HEAD
            | OPTIONS
            | PATCH
            | POST
            | PUT

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

  data Header = Accept             String
              | AcceptCharset      String
              | AcceptEncoding     String
              | AcceptLanguage     String
              | Allow              String
              | Authorization      String
              | CacheControl       String
              | Connection         String
              | ContentEncoding    String
              | ContentLanguage    String
              | ContentLength      String
              | ContentLocation    String
              | ContentMD5         String
              | ContentRange       String
              | ContentType        String
              | Date               String
              | Expect             String
              | Expires            String
              | From               String
              | Host               String
              | IfMatch            String
              | IfModifiedSince    String
              | IfNoneMatch        String
              | IfRange            String
              | IfUnmodifiedSince  String
              | LastModified       String
              | MaxForwards        String
              | Pragma             String
              | ProxyAuthorization String
              | Range              String
              | Referer            String
              | TE                 String
              | Trailer            String
              | TransferEncoding   String
              | Upgrade            String
              | UserAgent          String
              | Via                String
              | Warning            String

  instance showHTTPVerb :: Show Verb where
    show DELETE  = "DELETE"
    show GET     = "GET"
    show HEAD    = "HEAD"
    show OPTIONS = "OPTIONS"
    show PATCH   = "PATCH"
    show POST    = "POST"
    show PUT     = "PUT"

  instance showHeader :: Show Header where
    show header = header2Head header ++ ": " ++ header2Value header

  header2Head :: Header -> String
  header2Head (Accept _)             = "Accept"
  header2Head (AcceptCharset _)      = "Accept-Charset"
  header2Head (AcceptEncoding _)     = "Accept-Encoding"
  header2Head (AcceptLanguage _)     = "Accept-Language"
  header2Head (Allow _)              = "Allow"
  header2Head (Authorization _)      = "Authorization"
  header2Head (CacheControl _)       = "Cache-Control"
  header2Head (Connection _)         = "Connection"
  header2Head (ContentEncoding _)    = "Content-Encoding"
  header2Head (ContentLanguage _)    = "Content-Language"
  header2Head (ContentLength _)      = "Content-Length"
  header2Head (ContentLocation _)    = "Content-Location"
  header2Head (ContentMD5 _)         = "Content-MD5"
  header2Head (ContentRange _)       = "Content-Range"
  header2Head (ContentType _)        = "Content-Type"
  header2Head (Date _)               = "Date"
  header2Head (Expect _)             = "Expect"
  header2Head (Expires _)            = "Expires"
  header2Head (From _)               = "From"
  header2Head (Host _)               = "Host"
  header2Head (IfMatch _)            = "If-Match"
  header2Head (IfModifiedSince _)    = "If-Modified-Since"
  header2Head (IfNoneMatch _)        = "If-None-Match"
  header2Head (IfRange _)            = "If-Range"
  header2Head (IfUnmodifiedSince _)  = "If-Unmodified-Since"
  header2Head (LastModified _)       = "Last-Modified"
  header2Head (MaxForwards _)        = "Max-Forwards"
  header2Head (Pragma _)             = "Pragma"
  header2Head (ProxyAuthorization _) = "Proxy-Authorization"
  header2Head (Range _)              = "Range"
  header2Head (Referer _)            = "Referer"
  header2Head (TE _)                 = "Te"
  header2Head (Trailer _)            = "Trailer"
  header2Head (TransferEncoding _)   = "Transfer-Encoding"
  header2Head (Upgrade _)            = "Upgrade"
  header2Head (UserAgent _)          = "User-Agent"
  header2Head (Via _)                = "Via"
  header2Head (Warning _)            = "Warning"

  header2Value :: Header -> String
  header2Value (Accept v)             = v
  header2Value (AcceptCharset v)      = v
  header2Value (AcceptEncoding v)     = v
  header2Value (AcceptLanguage v)     = v
  header2Value (Allow v)              = v
  header2Value (Authorization v)      = v
  header2Value (CacheControl v)       = v
  header2Value (Connection v)         = v
  header2Value (ContentEncoding v)    = v
  header2Value (ContentLanguage v)    = v
  header2Value (ContentLength v)      = v
  header2Value (ContentLocation v)    = v
  header2Value (ContentMD5 v)         = v
  header2Value (ContentRange v)       = v
  header2Value (ContentType v)        = v
  header2Value (Date v)               = v
  header2Value (Expect v)             = v
  header2Value (Expires v)            = v
  header2Value (From v)               = v
  header2Value (Host v)               = v
  header2Value (IfMatch v)            = v
  header2Value (IfModifiedSince v)    = v
  header2Value (IfNoneMatch v)        = v
  header2Value (IfRange v)            = v
  header2Value (IfUnmodifiedSince v)  = v
  header2Value (LastModified v)       = v
  header2Value (MaxForwards v)        = v
  header2Value (Pragma v)             = v
  header2Value (ProxyAuthorization v) = v
  header2Value (Range v)              = v
  header2Value (Referer v)            = v
  header2Value (TE v)                 = v
  header2Value (Trailer v)            = v
  header2Value (TransferEncoding v)   = v
  header2Value (Upgrade v)            = v
  header2Value (UserAgent v)          = v
  header2Value (Via v)                = v
  header2Value (Warning v)            = v

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
