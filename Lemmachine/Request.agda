module Lemmachine.Request where
{-# IMPORT FFI #-}
{-# IMPORT Hack #-}
open import Data.Product
open import Data.List
open import Data.Maybe
open import Data.Bool
open import Data.Nat
open import Data.String

data Method : Set where
  HEAD GET PUT DELETE POST : Method
  TRACE CONNECT OPTIONS : Method

{-# COMPILED_DATA Method Hack.RequestMethod
    Hack.OPTIONS Hack.GET Hack.HEAD Hack.POST
    Hack.PUT Hack.DELETE Hack.TRACE Hack.CONNECT
#-}

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

Version = String
IP = String
LocalPath = String
Path = String

RawPath = String
PathToken = String
PathTokens = List PathToken

data RequestHeader : Set where
  _,_ : String → String → RequestHeader

{-# COMPILED_DATA RequestHeader FFI.RequestHeader FFI.RequestHeader #-}

headerKey : RequestHeader → String
headerKey (k , _) = k

RequestHeaders = List RequestHeader
Body = Maybe String
Cookie = String
QueryString = String
Port = String

record Request : Set where
  field
    method : Method
    version : Version
    peer : IP
    dispPath : LocalPath
    path : Path
    rawPath : RawPath
    pathTokens : PathTokens
    headers : RequestHeaders
    body : Body
    cookie : Cookie
    queryString : QueryString
    port : Port

postulate isRedirect : Request → Bool
