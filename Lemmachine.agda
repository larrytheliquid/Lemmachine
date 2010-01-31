module Lemmachine where
open import Data.Bool
open import Data.Nat
open import Data.String

data Request : Set where

data Method : Set where
  GET HEAD POST PUT DELETE : Method
  TRACE CONNECT OPTIONS : Method

Status = ℕ
data Headers : Set where
Body = String

record Response : Set where 
  field 
    status : Status 
    headers : Headers
    body : Body

forbidden : Request -> Bool
forbidden _ = false
