module Data where
open import Data.Bool
open import Data.Char
open import Data.Nat
open import Data.List hiding ([_])

infixr 2 _×_
infixr 5 _∷_
infixl 8 _^_

∞ : ℕ
∞ = 0

_^_ : ℕ → ℕ → ℕ
n ^ zero = 1
n ^ (suc m) = n * (n ^ (m))

data Decimal : ℕ → ℕ → Set where
  [_] : (n : ℕ) → Decimal 1 n
  _∷_ : ∀ {place val} → (n : ℕ) → Decimal place val → Decimal (suc place) (n * 10 ^ place + val)

data Single {A : Set} : A → Set where
  single : (x : A) → Single x

proj : {A : Set}{x : A} → Single x → A
proj (single x) = x

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

within? : Char → ℕ → ℕ → Bool
within? c start end = toBool lower ∧ toBool higher
  where
  target = toNat c
  lower  = suc target ∸ start
  higher = suc end ∸ target
  toBool : ℕ → Bool
  toBool zero    = false
  toBool (suc _) = true

data DarRange (start end : ℕ) : Bool → Set where
  dar : (c : Char) → DarRange start end (within? c start end)

data _×_ (A B : Set): Set where
  _,_ : A → B → A × B

data Dar : ℕ → Set where
  dar : (c : Char) → Dar (toNat c)
