module Spiky where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.Char
open import Data.Nat
open import Data.List
open import Data.Vec hiding (_>>=_)
open import Data.Sum
open import Data.Product hiding (_×_)
open import Data
open import Spike

infixr 1 _>>_ _>>-_ _>>=_

mutual 
  data U : Set where
    CHAR NAT : U
    DAR : ℕ → U
    DAR-RANGE : ℕ → ℕ → U
    SINGLE : (u : U) → El u → U
    VEC : U → ℕ → U
    METHOD REQUEST-URI HTTP-VERSION : U
    HEADER : Method → U
    VALUE : {m : Method} → Header m → U

  El : U → Set
  El CHAR = Char
  El NAT = ℕ
  El (DAR n) = Dar n
  El (DAR-RANGE n m) = DarRange n m true
  El (SINGLE _ x) = Single x
  El (VEC u n) = Vec (El u) n
  El METHOD = Method
  El REQUEST-URI = Request-URI
  El HTTP-VERSION = HTTP-Version
  El (HEADER m) = Header m
  El (VALUE h) = Value h

GET-HEADER  = HEADER GET
HEAD-HEADER = HEADER HEAD
POST-HEADER = HEADER POST

mutual
  data Format : Set where
    Fail End : Format
    Base : U → Format
    Somewhere : Format → Format
    Between : ℕ → ℕ → Format → Format
    Skip Or And : Format → Format → Format
    Use : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Fail ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base u ⟧ = El u
  ⟦ Somewhere f ⟧ = ⟦ f ⟧
  ⟦ Between x y f ⟧ = ⟦ f ⟧
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

char : Char → Format
char c = Base (DAR (toNat c))

DIGIT = Base (DAR-RANGE (toNat '0') (toNat '9'))
SP    = Base (DAR 32)
CR    = Base (DAR 13)
LF    = Base (DAR 10)
CRLF  = And CR LF

Value-Format : {m : Method} → Header m → Format
Value-Format {POST} Content-Length = Between 1 ∞ DIGIT
Value-Format _ = Fail

GET-Format : Format
GET-Format =
  Between 0 ∞ (
    Base GET-HEADER >>= λ h →
    char ':' >>
    SP >>
    Base (VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

HEAD-Format : Format
HEAD-Format =
  Between 0 ∞ (
    Base HEAD-HEADER >>= λ h →
    char ':' >>
    SP >>
    Base (VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

POST-Format : Format
POST-Format =
  Somewhere (
    Base (SINGLE POST-HEADER Content-Length) >>= λ c-l →
    char ':' >>
    SP >>
    Base (VALUE (proj c-l)) >>= λ n →
    CRLF >>
    
    f c-l n
  ) where
  f : (s : Single {Header POST} Content-Length) → Value (proj s) → Format
  f (single ._) n =
    Somewhere (
      Base (SINGLE POST-HEADER Content-Type) >>= λ h →
      char ':' >>
      SP >>
      Base (VALUE (proj h)) >>-
      CRLF >>
      End
    ) >>-

    Between 0 ∞ (
      Base POST-HEADER >>= λ h →
      char ':' >>
      SP >>
      Base (VALUE h) >>-
      CRLF >>
      End
    ) >>-

    CRLF >>
    Base (VEC CHAR n)

Method-Format : Method → Format
Method-Format GET  = GET-Format
Method-Format HEAD = HEAD-Format
Method-Format POST = POST-Format

Request-Format =
  Base METHOD >>= λ m →
  SP >>
  Base REQUEST-URI >>-
  SP >>
  Base HTTP-VERSION >>-
  CRLF >>  
  Method-Format m
