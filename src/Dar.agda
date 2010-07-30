module Dar where
open import Data.Char
open import Data.Nat

infix 4 _∈_
infixr 5 _∷_

data Dar : ℕ → Set where
  dar : (c : Char) → Dar (toNat c)

data Seq (start : ℕ) : ℕ → Set where
  [] : Seq start start
  _∷_ : ∀ {n} → Dar n → Seq start n → Seq start (suc n)

Char-Seq : Char → Char → Set
Char-Seq x y = Seq (toNat x) (suc (toNat y))

DIGITS : Char-Seq '0' '9'
DIGITS = (dar '9' ∷ dar '8' ∷ dar '7' ∷ dar '6' ∷ dar '5' ∷ dar '4' ∷ dar '3' ∷ dar '2' ∷ dar '1' ∷ dar '0' ∷ [])

data _∈_ {start : ℕ} : {n end : ℕ} → Dar n → Seq start end → Set where
  here : ∀ {end} {x : Dar end} {xs : Seq start end} → x ∈ x ∷ xs
  there : ∀ {end z} {x : Dar z} {y : Dar end} {xs : Seq start end} → x ∈ xs → x ∈ y ∷ xs

8-is-digit : dar '8' ∈ DIGITS
8-is-digit = there here
