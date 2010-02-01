module Lemmachine.Request where
open import Data.Bool
open import Data.String

AuthHead = Bool
Header = String
Path = String
MediaType = String
data Handler : Set where
  toHtml : Handler
Charset = String
data CharsetConverter : Set where
Encoding = String
data Encoder : Set where
  defaultEncoder : Encoder
data VaryHeader : Set where
MovedURI = String

data Method : Set where
  HEAD GET PUT DELETE POST : Method
  TRACE CONNECT OPTIONS : Method

data Request : Set where
