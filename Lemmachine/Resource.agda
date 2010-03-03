module Lemmachine.Resource where
open import Lemmachine.Resource.Config public
open import Lemmachine.Resource.Update
open import Lemmachine.Resource.Universe public
open import Data.List
open import Data.Product

_⇒_ : {A : Set}{B : A → Set} → (a : A) → B a → (Σ A B)
_⇒_ = _,_

configure₂ : Config → List (Σ Code convert) → Config
configure₂ base [] = base
configure₂ base ((code , f) ∷ cfs) = update code f (configure₂ base cfs)

configure : List (Σ Code convert) → Config
configure hooks = configure₂ default hooks
