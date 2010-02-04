module Lemmachine.Response where
open import Lemmachine.Status
open import Data.Product
open import Data.List
open import Data.String

ResponseHeader = String × String

record Response : Set where 
  field 
    status : ∀ {n} → Status n
    headers : List ResponseHeader
    body : String
