module Hmz where
open import Data.String

msg = "TRACE / HTTP/1.1\r\nHost: google.com\r\n\n"

parsed = toVec msg
