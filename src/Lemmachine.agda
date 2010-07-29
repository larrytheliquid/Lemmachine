module Lemmachine where
open import Data.String
open import Data.Maybe
open import Data.Product
open import Data.List

data Message : Set where
  end : Message
  line : String → String → Message → Message

data Method : Set where
  GET HEAD : Method

data Version : Set where
  1∶1 : Version

data Header : Set where
  method  : Method  → Header
  version : Version → Header
Headers = List Header

erase : Headers → Message
erase [] = end
erase ((method  GET) ∷ xs)      = line "method"  "GET" (erase xs)
erase ((method  HEAD) ∷ xs)     = line "method"  "HEAD" (erase xs)
erase ((version 1∶1) ∷ xs)      = line "version" "HTTP/1.1" (erase xs)

data Valid : Message → Set where
  valid : (xs : Headers) → Valid (erase xs)

check : (xs : Message) → Maybe (Valid xs)
check end = just (valid [])
check (line _ _ xs) with check xs
check (line "method"  "GET"  ._)     | just (valid ys) = just (valid (method GET ∷ ys))
check (line "method"  "HEAD" ._)     | just (valid ys) = just (valid (method HEAD ∷ ys))
check (line "version" "HTTP/1.1" ._) | just (valid ys) = just (valid (version 1∶1 ∷ ys))
... | _ = nothing
