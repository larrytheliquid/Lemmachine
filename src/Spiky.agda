module Spiky where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.Char
open import Data.String
open import Data.Nat
open import Data.List hiding ([_])
open import Data.Vec hiding (_>>=_; toList; [_])
open import Data.Sum
open import Data.Product hiding (_×_)
open import Data hiding ([_])
open import Spike

infixr 3 _∣_
infixr 1 _>>_ _>>-_ _>>=_

mutual 
  data U : Set where
    CHAR NAT : U
    DAR : ℕ → U
    DAR-RANGE : ℕ → ℕ → U
    SINGLE : (u : U) → El u → U
    STR : ℕ → U
    METHOD REQUEST-URI : U
    HEADER : Method → U
    VALUE : {m : Method} → Header m → U

  El : U → Set
  El CHAR = Char
  El NAT = ℕ
  El (DAR n) = Dar n
  El (DAR-RANGE n m) = DarRange n m true
  El (SINGLE _ x) = Single x
  El (STR n) = Vec Char n
  El METHOD = Method
  El REQUEST-URI = Request-URI
  El (HEADER m) = Header m
  El (VALUE h) = Value h

GET-HEADER  = HEADER GET
HEAD-HEADER = HEADER HEAD
POST-HEADER = HEADER POST

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

HTTP-Version-Format =
  str "HTTP" >>
  char '/' >> 
  Base NAT >>= λ major →
  char '.' >>
  Base NAT >>= λ minor →
  f major minor

  where

  f : ℕ → ℕ → Format
  f 0 9 = End
  f 1 0 = End
  f _ _ = Fail

GET-Format : Format
GET-Format =
  Slurp (
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
  Slurp (
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
  Upto End-Headers (
    Base (SINGLE POST-HEADER Content-Length) >>= λ h →
    char ':' >>
    SP >>
    Base (VALUE (proj h)) >>-
    CRLF >>
    End
  ) >>= λ c-l →

  f (Data.Product.proj₁ c-l) (Data.proj₁ (Data.Product.proj₂ c-l))

  where

  f : (s : Single {Header POST} Content-Length) → Value (proj s) → Format
  f (single ._) n =
    Upto End-Headers (
      Base (SINGLE POST-HEADER Content-Type) >>= λ h →
      char ':' >>
      SP >>
      Base (VALUE (proj h)) >>-
      CRLF >>
      End
    ) >>-

    Slurp (
      Base POST-HEADER >>= λ h →
      char ':' >>
      SP >>
      Base (VALUE h) >>-
      CRLF >>
      End
    ) >>-

    CRLF >>
    Base (STR n)

Method-Format : Method → Format
Method-Format GET  = GET-Format
Method-Format HEAD = HEAD-Format
Method-Format POST = POST-Format

Request-Format =
  Base METHOD >>= λ m →
  SP >>
  Base REQUEST-URI >>-
  SP >>
  HTTP-Version-Format >>-
  CRLF >>  
  Method-Format m
