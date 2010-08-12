module Parser where
open import Data.Unit
open import Data.Empty
open import Data.Char
open import Data.Sum
open import Data.Product

data U : Set where

mutual
  data Format : Set where
    Bad End : Format
    Base : U → Format
    Plus Skip : Format → Format → Format
    Read : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Bad ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base u ⟧ = ⊤
  ⟦ Plus f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ Read f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

-- http://tools.ietf.org/html/rfc2616#section-2.2

OCTET   = any-US-ASCII "8-bit sequence of data" 0 255
CHAR    = any-US-ASCII "character" 0 127
UPALPHA = any-US-ASCII "uppercase letter" 'A' 'Z'
LOALPHA = any-US-ASCII "lowercase letter" 'a' 'z'
ALPHA   = UPALPHA ∣ LOALPHA
DIGIT   = any-US-ASCII "digit" '0' '9'
CTL     = any-US-ASCII "control character" 0 31 ∣ US-ASCII "DEL" 127
CR      = US-ASCII "carriage return" 13
LF      = US-ASCII "linefeed" 10
SP      = US-ASCII "space" 32
HT      = US-ASCII "horizontal-tab" 9
DQ      = US-ASCII "double-quote mark" 34
