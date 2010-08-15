module Lemmachine where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.String hiding (_==_)
open import Data.Nat
open import Data.List
open import Data.Sum
open import Data.Product hiding (_×_)

infixr 1 _∣_ _>>=_ _>>-_ _>>_
infixr 2 _×_

data _×_ (A B : Set): Set where
  _,_ : A → B → A × B

∞ : ℕ
∞ = 0

_==_ : ℕ → ℕ → Bool
zero == zero = true
suc _ == zero = false
zero == suc _ = false
suc x == suc y = x == y

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

data HTTP : Set where
  DAR : ℕ → HTTP

  Method : HTTP
  Request-URI : HTTP

  HTTP-Version : HTTP

  Date Pragma : HTTP

  Authorization From : HTTP
  If-Modified-Since : HTTP
  Referer User-Agent : HTTP

  Allow Content-Encoding : HTTP
  Content-Length Content-Type : HTTP
  Expires Last-Modified : HTTP

Type : HTTP → Set
Type _ = String

mutual
  data Format : Set where
    Fail End : Format
    Base : HTTP → Format
    Between : ℕ → ℕ → Format → Format
    Or And Skip : Format → Format → Format
    Use : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Fail ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base x ⟧ = Type x
  ⟦ Between x y f ⟧ = RList ⟦ f ⟧ x y
  ⟦ Or f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ And f₁ f₂ ⟧ = ⟦ f₁ ⟧ × ⟦ f₂ ⟧
  ⟦ Use f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

_>>_ : Format → Format → Format
f₁ >> f₂ = Skip f₁ f₂

_>>=_ : (f : Format) → (⟦ f ⟧ → Format) → Format
x >>= y = Use x y

_>>-_ : Format → Format → Format
x >>- y = And x y

data U : Set where
  http format : U

El : U → Set
El http   = HTTP
El format = Format

_∣_ : {x y : U} → El x → El y → Format
_∣_ {http}   {http}   x y = Or (Base x) (Base y)
_∣_ {http}   {format} x y = Or (Base x) y
_∣_ {format} {format} x y = Or x y
_∣_ {format} {http}   x y = Or x (Base y)

SP   = Base (DAR 32)
CR   = Base (DAR 13)
LF   = Base (DAR 10)
CRLF = CR >>- LF

Request-Line =
  Base Method >>-
  SP >>
  Base Request-URI >>-
  SP >>
  Base HTTP-Version >>-
  CRLF >>
  End

General-Header = Date
               ∣ Pragma

Request-Header = Authorization
               ∣ From
               ∣ If-Modified-Since
               ∣ Referer
               ∣ User-Agent

Entity-Header = Allow
              ∣ Content-Encoding
              ∣ Content-Length
              ∣ Content-Type
              ∣ Expires
              ∣ Last-Modified
