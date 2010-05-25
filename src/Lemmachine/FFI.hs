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

data ResponseHeader = ResponseHeader String String
type ResponseHeaders = [ResponseHeader]

data Response = Response {
  status :: String,
  respHeaders :: ResponseHeaders,
  respBody :: String
  }

run :: (Request -> Response) -> IO ()
run f = Handler.run $ return . \env -> 
  let response = f $ toRequest env in
  Hack.Response { 
    Hack.status = read $ status response,
    Hack.headers = map fromHeader $ respHeaders response, 
    Hack.body = pack $ respBody response
    }
    where fromHeader (ResponseHeader k v) = (k , v)



