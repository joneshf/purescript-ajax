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

  data Header = Header HeaderHead String

  -- This type is not expressive enough.
  data HeaderHead = Accept
                  | AcceptCharset
                  | AcceptEncoding
                  | AcceptLanguage
                  | Allow
                  | Authorization
                  | CacheControl
                  | Connection
                  | ContentEncoding
                  | ContentLanguage
                  | ContentLength
                  | ContentLocation
                  | ContentMD5
                  | ContentRange
                  | ContentType
                  | Date
                  | Expect
                  | Expires
                  | From
                  | Host
                  | IfMatch
                  | IfModifiedSince
                  | IfNoneMatch
                  | IfRange
                  | IfUnmodifiedSince
                  | LastModified
                  | MaxForwards
                  | Pragma
                  | ProxyAuthorization
                  | Range
                  | Referer
                  | TE
                  | Trailer
                  | TransferEncoding
                  | Upgrade
                  | UserAgent
                  | Via
                  | Warning
                  | Custom String

  instance showHTTPVerb :: Show Verb where
    show DELETE  = "DELETE"
    show GET     = "GET"
    show HEAD    = "HEAD"
    show OPTIONS = "OPTIONS"
    show PATCH   = "PATCH"
    show POST    = "POST"
    show PUT     = "PUT"

  instance showHeader :: Show Header where
    show (Header head value) = show head ++ ": " ++ value

  -- TODO: StatusCode need a show instance

  instance showHeaderHead :: Show HeaderHead where
    show Accept             = "Accept"
    show AcceptCharset      = "Accept-Charset"
    show AcceptEncoding     = "Accept-Encoding"
    show AcceptLanguage     = "Accept-Language"
    show Allow              = "Allow"
    show Authorization      = "Authorization"
    show CacheControl       = "Cache-Control"
    show Connection         = "Connection"
    show ContentEncoding    = "Content-Encoding"
    show ContentLanguage    = "Content-Language"
    show ContentLength      = "Content-Length"
    show ContentLocation    = "Content-Location"
    show ContentMD5         = "Content-MD5"
    show ContentRange       = "Content-Range"
    show ContentType        = "Content-Type"
    show Date               = "Date"
    show Expect             = "Expect"
    show Expires            = "Expires"
    show From               = "From"
    show Host               = "Host"
    show IfMatch            = "If-Match"
    show IfModifiedSince    = "If-Modified-Since"
    show IfNoneMatch        = "If-None-Match"
    show IfRange            = "If-Range"
    show IfUnmodifiedSince  = "If-Unmodified-Since"
    show LastModified       = "Last-Modified"
    show MaxForwards        = "Max-Forwards"
    show Pragma             = "Pragma"
    show ProxyAuthorization = "Proxy-Authorization"
    show Range              = "Range"
    show Referer            = "Referer"
    show TE                 = "Te"
    show Trailer            = "Trailer"
    show TransferEncoding   = "Transfer-Encoding"
    show Upgrade            = "Upgrade"
    show UserAgent          = "User-Agent"
    show Via                = "Via"
    show Warning            = "Warning"
    show (Custom header)    = header

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
