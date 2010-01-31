module Lemmachine where
open import Lemmachine.Status
open import Data.Bool
open import Data.String

data Request : Set where

data Method : Set where
  GET HEAD POST PUT DELETE : Method
  TRACE CONNECT OPTIONS : Method

data Headers : Set where

Body = String

data Default (A : Set) : Set where
  default : Default A
  value : A -> Default A

record Response : Set where 
  field 
    status : ∀ {n} -> Status n
    headers : Headers
    body : Body

data Resource : Set where

forbidden : Request -> Bool
forbidden _ = false
