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
  just : (xs : Headers) → Infer (erase xs)
  nothing : {ys : Raws} → Infer ys

infer : (xs : Raws) → Infer xs
infer [] = just []
infer (x ∷ xs) with infer xs
infer (method "GET" ∷ ._) | just ys = just (method GET ∷ ys)
infer (method "HEAD" ∷ ._) | just ys = just (method HEAD ∷ ys)
infer (version y″ ∷ ._) | just ys = just (version y″ ∷ ys)
... | _ = nothing

