module Lemmachine.Request where
{-# IMPORT FFI #-}
{-# IMPORT Hack #-}
open import Data.Product
open import Data.List
open import Data.Maybe
open import Data.Bool
open import Data.Nat
open import Data.String

AuthHead = Bool
Path = String
MediaType = String
Handler = String
Charset = String
CharsetConverter = String
Encoding = String
Encoder = String
MovedURI = String
DateTime = String
ETag = String

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

data Data-Request : Set where
  request :
    Method →
    Version →
    IP →
    LocalPath →
    Path →
    RawPath →
    PathTokens →
    RequestHeaders →
    Body →
    Cookie →
    QueryString →
    Port →
    Data-Request
{-# COMPILED_DATA Data-Request FFI.Request FFI.Request #-}

toRequest : Data-Request → Request
toRequest (request 
  method
  version
  peer
  dispPath
  path
  rawPath
  pathTokens
  headers
  body
  cookie
  queryString
  port
  ) = record {
    method = method
  ; version = version
  ; peer = peer
  ; dispPath = dispPath
  ; path = path
  ; rawPath = rawPath
  ; pathTokens = pathTokens
  ; headers = headers
  ; body = body
  ; cookie = cookie
  ; queryString = queryString
  ; port = port
  }

postulate isRedirect : Request → Bool
