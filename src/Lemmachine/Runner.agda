module Lemmachine.Runner where
{-# IMPORT Lemmachine.FFI #-}
open import Lemmachine.Resource
open import Lemmachine.Resource.Configure
open import Lemmachine.Request
open import Lemmachine.Response
open import Lemmachine.Utils
open import Lemmachine.Resolve
open import Data.Unit
open import Data.Nat
open import Data.String
open import Data.Function
open import Foreign.Haskell
open import IO.Primitive

private
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

  {-# COMPILED_DATA Data-Request 
      Lemmachine.FFI.Request 
      Lemmachine.FFI.Request
  #-}

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
    ; reqBody = body
    ; cookie = cookie
    ; queryString = queryString
    ; port = port
    }

  data Data-Response : Set where
    response : String → ResponseHeaders → String → Data-Response

  {-# COMPILED_DATA Data-Response
      Lemmachine.FFI.Response 
      Lemmachine.FFI.Response
  #-}

  fromResponse : Response → Data-Response
  fromResponse resp = response
    (showStatus $ Response.status resp)
    (Response.headers resp)
    (Response.body resp)

  Data-resolve : Application → Data-Request → Data-Response
  Data-resolve app req = fromResponse $ app (toRequest req)

  postulate run : (Data-Request → Data-Response) → IO Unit
  {-# COMPILED run Lemmachine.FFI.run #-}

runResolve : Application → IO Unit
runResolve app = run $ Data-resolve app
