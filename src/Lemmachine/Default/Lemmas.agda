module Lemmachine.Default.Lemmas where
open import Lemmachine
import Lemmachine.Default
import Lemmachine.Lemmas
open Lemmachine.Lemmas Lemmachine.Default.resource
open import Relation.Binary.PropositionalEquality
open import Data.Empty
open import Data.Maybe
open import Data.Product hiding (map)
open import Data.Function using (const)
open import Data.List.Any hiding (any)

methodAlwaysKnown : ∀ r → any (eqMethod (method r))
                          (knownMethods r) ≡ true
methodAlwaysKnown r with method r
... | HEAD = refl
... | GET = refl
... | PUT = refl
... | DELETE = refl
... | POST = refl
... | TRACE = refl
... | CONNECT = refl
... | OPTIONS = refl

