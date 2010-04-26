module FFI where
import Hack
import Hack.Handler.Happstack as Handler
import Data.ByteString.Lazy.Char8 (pack)

data Status =
  OK | Created | Accepted | NoContent |
  MultipleChoices | MovedPermanently | SeeOther | NotModified | MovedTemporarily |
  BadRequest | Unauthorized | Forbidden | NotFound | MethodNotAllowed |
  NotAcceptable | Conflict | Gone | PreconditionFailed |
  RequestEntityTooLarge | RequestURItooLong | UnsupportedMediaType |
  NotImplemented | ServiceUnavailable

fromStatus :: Status -> Int
fromStatus OK = 200
fromStatus Created = 201
fromStatus Accepted = 202
fromStatus NoContent = 204
fromStatus MultipleChoices = 300
fromStatus MovedPermanently = 301
fromStatus SeeOther = 303
fromStatus NotModified = 304
fromStatus MovedTemporarily = 307
fromStatus BadRequest = 400
fromStatus Unauthorized = 401
fromStatus Forbidden = 403
fromStatus NotFound = 404
fromStatus MethodNotAllowed = 405
fromStatus NotAcceptable = 406
fromStatus Conflict = 409
fromStatus Gone = 410
fromStatus PreconditionFailed = 412
fromStatus RequestEntityTooLarge = 413
fromStatus RequestURItooLong = 414
fromStatus UnsupportedMediaType = 415
fromStatus NotImplemented = 501
fromStatus ServiceUnavailable = 503
            
run :: Status -> IO ()
run status = Handler.run $ return . \env -> Response 
    { status  = code
    , headers = [ ("Content-Type", "text/plain") ]
    , body    = pack ("This HTTP status (" ++ show code ++ ") is brought to you by Lemmachine!")
    }
      where code = fromStatus status
