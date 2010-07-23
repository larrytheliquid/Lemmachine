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

data Infer : Raws → Set where
  ok : (xs : Headers) → Infer (erase xs)

infer : (xs : Raws) → Maybe (Infer xs)
infer [] = just (ok [])
infer (x ∷ xs) with infer xs
infer (method "GET" ∷ ._) | just (ok ys) = just (ok (method GET ∷ ys))
infer (method "HEAD" ∷ ._) | just (ok ys) = just (ok (method HEAD ∷ ys))
infer (version y″ ∷ ._) | just (ok ys) = just (ok (version y″ ∷ ys))
... | _ = nothing

