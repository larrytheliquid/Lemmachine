module Sneaky where
open import Data.Bool
open import Data.Nat
open import Data.List
open import Data.Vec

data BoolPair : Set where
  _,_ : Bool → Bool → BoolPair

data NatPair : Set where
  _a,_ : ℕ → ℕ → NatPair

-- El : Bool → Set
-- El true = BoolPair
-- El false = NatPair

-- Hmz : {b : Bool} → Set
-- Hmz {b} = El b

-- testing : Hmz
-- testing = 0 a, 1

El : Bool → Set
El true = List ℕ
El false = (n : ℕ) → Vec Bool n

Hmz : {b : Bool} → Set
Hmz {b} = El b

testing : Hmz {true}
testing = 0 ∷ 1 ∷ 2 ∷ []

test : (El false)
test n = true ∷ false ∷ []
