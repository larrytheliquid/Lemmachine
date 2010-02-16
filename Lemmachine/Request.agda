module Lemmachine.Request where
open import Data.Product
open import Data.List
open import Data.Maybe
open import Data.Bool
open import Data.Nat
open import Data.String

AuthHead = Bool
Path = String
RequestHeader = String × String
MediaType = String
Handler = String
-- data Handler : Set where
--   toHtml : Handler
Charset = String
data CharsetConverter : Set where
Encoding = String
data Encoder : Set where
  defaultEncoder : Encoder
data VaryHeader : Set where
MovedURI = String
data DateTime : Set where
data ETag : Set where

data Method : Set where
  HEAD GET PUT DELETE POST : Method
  TRACE CONNECT OPTIONS : Method

eqMethod : Method → Method → Bool
eqMethod HEAD HEAD = true
eqMethod GET GET = true
eqMethod PUT PUT = true
eqMethod DELETE DELETE = true
eqMethod POST POST = true
eqMethod TRACE TRACE = true
eqMethod CONNECT CONNECT = true
eqMethod OPTIONS OPTIONS = true
eqMethod _ _ = false

Version = ℕ × ℕ
IP = String
LocalPath = String

RawPath = String
Cookie = String
QueryString = String
Port = String

-- TODO: pathInfo
-- TODO: make Headers a Map?
record Request : Set where
  field
    method : Method
    version : Version
    peer : IP
    dispPath : LocalPath
    path : Path
    rawPath : RawPath
    pathTokens : List String
    headers : List RequestHeader
    body : Maybe String
    cookie : Cookie
    queryString : QueryString
    port : Port
