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

data Version : Set where
  HTTP-1-1 : Version

-- maybe Method as an index?
data Header : Set where
  method  : Method  → Header
  version : Version → Header
Headers = List Header

erase : Headers → Raws
erase [] = []
erase ((method  GET) ∷ xs)      = method  "GET" ∷ (erase xs)
erase ((method  HEAD) ∷ xs)     = method  "HEAD" ∷ (erase xs)
erase ((version HTTP-1-1) ∷ xs) = version "HTTP/1.1" ∷ (erase xs)

data Valid : Raws → Set where
  valid : (xs : Headers) → Valid (erase xs)

check : (xs : Raws) → Maybe (Valid xs)
check [] = just (valid [])
check (x ∷ xs) with check xs
check (method  "GET" ∷ ._)      | just (valid ys) = just (valid (method GET ∷ ys))
check (method  "HEAD" ∷ ._)     | just (valid ys) = just (valid (method HEAD ∷ ys))
check (version "HTTP/1.1" ∷ ._) | just (valid ys) = just (valid (version HTTP-1-1 ∷ ys))
... | _ = nothing
