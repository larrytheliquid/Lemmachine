module BasicRules where
open import Data.Bool
open import Data.Char
open import Data.String
open import Data.Nat
open import Data.Sum

infixr 1 _∣_

_∣_ : (A B : Set) → Set
A ∣ B = A ⊎ B

data U : Set where
  DAR NAT : U

El : U → Set
El DAR = Char
El NAT = ℕ

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

data AnyDar (start end : ℕ) : Bool → Set where
  char : (c : Char) → AnyDar start end (between? c start end)

US-ASCII : {u : U} → String → El u → Set
US-ASCII {DAR} _ x = Dar (toNat x)
US-ASCII {NAT} _ x = Dar x

any-US-ASCII : {u : U} → String → El u → El u → Set
any-US-ASCII {DAR} _ x y = AnyDar (toNat x) (toNat y) true
any-US-ASCII {NAT} _ x y = AnyDar x y true

OCTET   = any-US-ASCII "8-bit sequence of data" 0 255
CHAR    = any-US-ASCII "character" 0 127
UPALPHA = any-US-ASCII "uppercase" 'A' 'Z'
LOALPHA = any-US-ASCII "lowercase" 'a' 'z'
ALPHA   = UPALPHA ∣ LOALPHA
DIGIT   = any-US-ASCII "digit" '0' '9'
CTL     = any-US-ASCII "control character" 0 31 ∣ US-ASCII "DEL" 127
CR      = US-ASCII "carriage return" 13
LF      = US-ASCII "linefeed" 10
SP      = US-ASCII "space" 32
HT      = US-ASCII "horizontal-tab" 9
DQ      = US-ASCII "double-quote mark" 34

