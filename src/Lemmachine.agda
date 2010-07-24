module Lemmachine where
open import Data.String
open import Data.Maybe
open import Data.Product
open import Data.List

data Raw : Set where
  method version : String → Raw
Raws = List Raw

data Method : Set where
  GET HEAD : Method

-- maybe Method as an index?
data Header : Set where
  method : Method → Header
  version : String → Header
Headers = List Header

erase : Headers → Raws
erase [] = []
erase (x ∷ xs) = erase″ x ∷ (erase xs)
  where
  erase″ : Header → Raw
  erase″ (method GET) = method "GET"
  erase″ (method HEAD) = method "HEAD"
  erase″ (version x) = version x

data Valid : Raws → Set where
  valid : (xs : Headers) → Valid (erase xs)

check : (xs : Raws) → Maybe (Valid xs)
check [] = just (valid [])
check (x ∷ xs) with check xs
check (method "GET" ∷ ._)  | just (valid ys) = just (valid (method GET ∷ ys))
check (method "HEAD" ∷ ._) | just (valid ys) = just (valid (method HEAD ∷ ys))
check (version y ∷ ._)    | just (valid ys) = just (valid (version y ∷ ys))
... | _ = nothing
