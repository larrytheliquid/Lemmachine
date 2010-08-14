module Lemmachine where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.String hiding (_==_)
open import Data.Nat
open import Data.List
open import Data.Sum
open import Data.Product

infixr 1 _∣_

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
    Bad End : Format
    Base : HTTP → Format
    Between : ℕ → ℕ → Format → Format
    Plus Skip : Format → Format → Format
    Read : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Bad ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base x ⟧ = Type x
  ⟦ Between x y f ⟧ = RList ⟦ f ⟧ x y
  ⟦ Plus f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ Read f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

data U : Set where
  http format : U

El : U → Set
El http   = HTTP
El format = Format

_∣_ : {x y : U} → El x → El y → Format
_∣_ {http}   {http}   x y = Plus (Base x) (Base y)
_∣_ {http}   {format} x y = Plus (Base x) y
_∣_ {format} {format} x y = Plus x y
_∣_ {format} {http}   x y = Plus x (Base y)

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
