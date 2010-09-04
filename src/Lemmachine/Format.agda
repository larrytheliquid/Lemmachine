module Lemmachine.Format where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.Char
open import Data.String
open import Data.Nat
open import Data.List hiding ([_])
open import Data.Vec hiding (_>>=_; toList; [_])
open import Data.Sum
open import Data.Product
open import Lemmachine.Data hiding ([_])
open import Lemmachine.HTTP

infixr 3 _∣_
infixr 1 _>>_ _>>-_ _>>=_

data U : Set where
  CHAR NAT : U
  DAR : ℕ → U
  DAR-RANGE : ℕ → ℕ → U
  SINGLE : Header-Name → U
  STR : ℕ → U
  VERSION : U
  METHOD CODE : U
  REQUEST-URI REASON-PHRASE : U
  HEADER-NAME : U
  HEADER-VALUE : Header-Name → U

El : U → Set
El CHAR = Char
El NAT = ℕ
El (DAR n) = Dar n
El (DAR-RANGE n m) = DarRange n m true
El (SINGLE x) = Single x
El (STR n) = Vec Char n
El VERSION = Version
El METHOD = Method
El CODE = Code
El REQUEST-URI = Request-URI
El REASON-PHRASE = Reason-Phrase
El HEADER-NAME = Header-Name
El (HEADER-VALUE h) = Header-Value h

mutual
  data Format : Set where
    Fail End : Format
    Base : U → Format
    Upto : Format → Format → Format
    Slurp : Format → Format
    Skip Or And : Format → Format → Format
    Use : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Fail ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base u ⟧ = El u
  ⟦ Upto _ f ⟧ = ⟦ f ⟧
  ⟦ Slurp f ⟧ = List ⟦ f ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ Or f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ And f₁ f₂ ⟧ = ⟦ f₁ ⟧ × ⟦ f₂ ⟧
  ⟦ Use f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

_>>_ : Format → Format → Format
f₁ >> f₂ = Skip f₁ f₂

_>>-_ : Format → Format → Format
x >>- y = And x y

_>>=_ : (f : Format) → (⟦ f ⟧ → Format) → Format
x >>= y = Use x y

_∣_ : Format → Format → Format
x ∣ y = Or x y

char : Char → Format
char c = Base (DAR (toNat c))

str : String → Format
str s = chars (toList s)
  where
  chars : List Char → Format
  chars [] = End
  chars (x ∷ xs) = char x >>- chars xs

DIGIT = Base (DAR-RANGE (toNat '0') (toNat '9'))
SP    = Base (DAR 32)
CR    = Base (DAR 13)
LF    = Base (DAR 10)
CRLF  = CR >>- LF
End-Headers  = CRLF >>- CRLF

Required-Header : Header-Name → Format
Required-Header h =
  Upto End-Headers (
    Base (SINGLE h) >>= λ h →
    char ':' >>
    SP >>
    Base (HEADER-VALUE (proj h)) >>-
    CRLF >>
    End
  )

Optional-Header : Header-Name → Format
Optional-Header h = Required-Header h ∣ End

Disallow-Other-Headers : Format
Disallow-Other-Headers =
  Or (
    Base HEADER-NAME >>= λ h →
    char ':' >>
    SP >>
    Base (HEADER-VALUE h) >>-
    CRLF >>
    Fail
  ) End
