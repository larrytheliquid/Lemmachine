module Lemmachine.FFI where
import qualified Hack
import qualified Hack.Handler.Happstack as Handler
import Data.ByteString.Lazy.Char8 (pack)

type Version = String
type IP = String
type LocalPath = String
type Path = String
type RawPath = String
type PathToken = String
type PathTokens = [PathToken]
data RequestHeader = RequestHeader String String
type RequestHeaders = [RequestHeader]
type Body = Maybe String
type Cookie = String
type QueryString = String
type Port = String

data Request = Request {
  method :: Hack.RequestMethod,
  version :: Version,
  peer :: IP,
  dispPatch :: LocalPath,
  path :: Path,
  rawPath :: RawPath,
  pathTokens :: PathTokens,
  headers :: RequestHeaders,
  body :: Body,
  cookie :: Cookie,
  queryString :: QueryString,
  port :: Port
  }

data Status =
  OK | Created | Accepted | NoContent |
  MultipleChoices | MovedPermanently | SeeOther | NotModified | MovedTemporarily |
  BadRequest | Unauthorized | Forbidden | NotFound | MethodNotAllowed |
  NotAcceptable | Conflict | Gone | PreconditionFailed |
  RequestEntityTooLarge | RequestURItooLong | UnsupportedMediaType |
  NotImplemented | ServiceUnavailable

statusCode :: Status -> Int
statusCode OK = 200
statusCode Created = 201
statusCode Accepted = 202
statusCode NoContent = 204
statusCode MultipleChoices = 300
statusCode MovedPermanently = 301
statusCode SeeOther = 303
statusCode NotModified = 304
statusCode MovedTemporarily = 307
statusCode BadRequest = 400
statusCode Unauthorized = 401
statusCode Forbidden = 403
statusCode NotFound = 404
statusCode MethodNotAllowed = 405
statusCode NotAcceptable = 406
statusCode Conflict = 409
statusCode Gone = 410
statusCode PreconditionFailed = 412
statusCode RequestEntityTooLarge = 413
statusCode RequestURItooLong = 414
statusCode UnsupportedMediaType = 415
statusCode NotImplemented = 501
statusCode ServiceUnavailable = 503

toRequest :: Hack.Env -> Request
toRequest e = Request {
  method = Hack.requestMethod e,
  version = show $ Hack.hackVersion e,
  peer = Hack.remoteHost e,
  dispPatch = Hack.pathInfo e,
  path = Hack.pathInfo e,
  rawPath = Hack.pathInfo e,
  pathTokens = [],
  headers = map toHeader $ Hack.http e,
  body = Just (show $ Hack.hackInput e),
  cookie = "",
  queryString = Hack.queryString e,
  port = show $ Hack.serverPort e
  }
  where toHeader h = RequestHeader (fst h) (snd h)

run :: (Request -> Status) -> IO ()
run f = Handler.run $ return . \env -> 
  let code = statusCode $ f (toRequest env) in
  Hack.Response 
    { Hack.status = code
    , Hack.headers = [ ("Content-Type", "text/html") ]
    , Hack.body = pack $ toHtml code
    }
      where
        toHtml x = doctype ++ html x
        doctype = "<!DOCTYPE html>"
        html x = "<html lang='en'>" ++ head ++ body x ++ "</html>"
        head = "<head><meta charset=utf-8 /><title>Lemmachine</title></head>"
        body x = "<body>This HTTP status (" ++ show x ++ ") is brought to you by Lemmachine!</body>"

