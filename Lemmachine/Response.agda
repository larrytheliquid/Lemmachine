module Lemmachine.Response where
open import Lemmachine.Status
open import Data.String

data Headers : Set where

Body = String

record Response : Set where 
  field 
    status : ∀ {n} -> Status n
    headers : Headers
    body : Body
