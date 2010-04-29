module Lemmachine.Runner where
{-# IMPORT Lemmachine.FFI #-}
open import Lemmachine
import Lemmachine.Resource.Default as Default
open import Data.Unit
open import Data.Nat
open import Data.String
open import Data.Function
open import Foreign.Haskell
open import IO.Primitive

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

Data-resolve : Resource → Data-Request → Status
Data-resolve res req = resolve res $ toRequest req

postulate run : (Data-Request → Status) → IO Unit

{-# COMPILED run Lemmachine.FFI.run #-}

main = run $ Data-resolve (toResource Default.resource)

