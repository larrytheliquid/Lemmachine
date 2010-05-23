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
    ; body = body
    ; cookie = cookie
    ; queryString = queryString
    ; port = port
    }

  Data-resolve : Application → Data-Request → Status
  Data-resolve app req = app $ toRequest req

  postulate run : (Data-Request → Status) → IO Unit
  {-# COMPILED run Lemmachine.FFI.run #-}

runResolve : Application → IO Unit
runResolve app = run $ Data-resolve app
