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
open import Spike

infixr 1 _>>_ _>>-_ _>>=_
infixr 2 _×_

∞ : ℕ
∞ = 0

data BList (A : Set) : ℕ → Set where
  [] : ∀ {n} → BList A n
  _∷_ : ∀ {n} → A → BList A n → BList A (suc n)

data Cons (A B : Set) : Set where
  _∷_ : A → B → Cons A B

RList : Set → ℕ → ℕ → Set
RList A zero (suc m)    = BList A (suc m)
RList A zero ∞          = List A
RList A (suc n) zero    = Cons A (RList A n zero)
RList A (suc n) (suc m) = Cons A (RList A n m)

between? : Char → ℕ → ℕ → Bool
between? c start end = toBool lower ∧ toBool higher
  where
  target = toNat c
  lower  = suc target ∸ start
  higher = suc end ∸ target
  toBool : ℕ → Bool
  toBool zero    = false
  toBool (suc _) = true

data DarRange (start end : ℕ) : Bool → Set where
  dar : (c : Char) → DarRange start end (between? c 0 end)

data _×_ (A B : Set): Set where
  _,_ : A → B → A × B

data Dar : ℕ → Set where
  dar : (c : Char) → Dar (toNat c)

data U : Set where
  CHAR NAT METHOD : U
  HEADER : (m : Method) → Header m → U
  VALUE : {m : Method}{h : Header m} → HeaderSingle h → U
  DAR : ℕ → U
  DAR-RANGE : ℕ → ℕ → U
  VEC : U → ℕ → U

El : U → Set
El CHAR = Char
El NAT = ℕ
El METHOD = Method
El (HEADER _ h) = HeaderSingle h
El (VALUE h) = Value h
El (DAR n) = Dar n
El (DAR-RANGE n m) = DarRange n m true
El (VEC u n) = Vec (El u) n

GET-HEADER  = HEADER GET
HEAD-HEADER = HEADER HEAD
POST-HEADER = HEADER POST

mutual
  data Format : Set where
    Fail End : Format
    Base : U → Format
    Somewhere : Format → Format
    Between : ℕ → ℕ → Format → Format
    Or And Skip : Format → Format → Format
    Use : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Fail ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base u ⟧ = El u
  ⟦ Somewhere f ⟧ = ⟦ f ⟧
  ⟦ Between x y f ⟧ = RList ⟦ f ⟧ x y
  ⟦ Or f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
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
GET-Format = Fail

HEAD-Format : Format
HEAD-Format = Fail

POST-Format : Format
POST-Format =
  Base (POST-HEADER Content-Length) >>= λ c-l →
  char ':' >>
  Base (VALUE c-l) >>= λ n →

  Base (POST-HEADER Content-Type) >>= λ c-t →
  char ':' >>
  Base (VALUE c-t) >>-

  body c-l n

  where

  body : (h : HeaderSingle Content-Length) → Value h → Format
  body (header ._) v = Base (VEC CHAR v)

Method-Format : Method → Format
Method-Format GET  = GET-Format
Method-Format HEAD = HEAD-Format
Method-Format POST = POST-Format

Request-Format =
  Base METHOD >>= λ m →
  CRLF >>
  Method-Format m
