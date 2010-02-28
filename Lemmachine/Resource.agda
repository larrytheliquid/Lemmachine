module Lemmachine.Resource where
open import Lemmachine.Resource.Config public
open import Lemmachine.Resource.Update
open import Lemmachine.Resource.Universe
open import Data.List
open import Data.Product

configure : List (Σ Code convert) → Config
configure [] = default
configure ((code , f) ∷ cfs) = update code f (configure cfs)
