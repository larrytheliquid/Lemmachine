import Hack
import Hack.Handler.Happstack
import Data.ByteString.Lazy.Char8 (pack)

statusToApp :: Int -> Application
statusToApp status env = return $ Response 
    { status  = status
    , headers = [ ("Content-Type", "text/plain") ]
    , body    = pack "This HTTP status is brought to you by Lemmachine!"
    }

main = run $ statusToApp 200
