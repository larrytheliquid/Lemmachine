module Grammar where
open import Data.Char
open import Data.Nat

data US-ASCII : ℕ → Set where
  rule : (c : Char) → US-ASCII (toNat c)

LF   = US-ASCII 10
SP   = US-ASCII 32
HT   = US-ASCII 9
<''> = US-ASCII 34
