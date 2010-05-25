module Lemmachine.Default.Proofs where
open import Lemmachine
import Lemmachine.Default
import Lemmachine.Lemmas
open import Lemmachine.Default.Lemmas
open Lemmachine.Lemmas Lemmachine.Default.resource
open import Relation.Binary.PropositionalEquality
open import Data.Empty
open import Data.Maybe
open import Data.Product hiding (map)
open import Data.Function using (const)
open import Data.List.Any

methodNotAllowed : ∀ r → method r ∉ allowedMethods r
                       → resolveStatus r ≡ MethodNotAllowed
methodNotAllowed r p with methodAlwaysKnown r | methodIsntAllowed r p
... | p₂ | p₃ rewrite p₂ | p₃ = refl

optionsOK : ∀ r → method r ∈ allowedMethods r
                → method r ≡ OPTIONS
                → resolveStatus r ≡ OK
optionsOK r p p₂ with methodAlwaysKnown r | methodIsAllowed r p
... | p₃ | p₄  rewrite p₃ | p₄ | p₂ = refl
