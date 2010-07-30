module Dar2 where
open import Data.Bool
open import Data.Char
open import Data.Nat
open import Data.Sum

data Dar : ℕ → Set where
  char : (c : Char) → Dar (toNat c)

between? : Char → ℕ → ℕ → Bool
between? c start end = toBool lower ∧ toBool higher
  where
  target = toNat c
  lower  = suc target ∸ start
  higher = suc end ∸ target
  toBool : ℕ → Bool
  toBool zero    = false
  toBool (suc _) = true

data Between (start end : ℕ) : Bool → Set where
  char : (c : Char) → Between start end (between? c start end)

data U : Set where
  DAR NAT : U

El : U → Set
El DAR = Char
El NAT = ℕ

IsBetween : {u : U} → El u → El u → Set
IsBetween {DAR} x y = Between (toNat x) (toNat y) true
IsBetween {NAT} x y = Between x y true

OCTET = IsBetween 0 255
CHAR = IsBetween 0 127
UPALPHA = IsBetween 'A' 'Z'
LOALPHA = IsBetween 'a' 'z'
ALPHA = UPALPHA ⊎ LOALPHA
DIGIT = IsBetween '0' '9'
CTL = IsBetween 0 31 ⊎ Dar 127
CR = Dar 13
LF = Dar 10
SP = Dar 32
HT = Dar 9
DQ = Dar 34

