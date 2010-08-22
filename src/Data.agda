module Data where
open import Data.Bool
open import Data.Char
open import Data.Nat
open import Data.Maybe
open import Data.List hiding ([_]; drop)
open import Data.Vec hiding ([_])

infixr 2 _×_
infixr 5 _∷_
infixl 8 _^_

∞ : ℕ
∞ = 0

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

proj₁ : ∀ {A B} → A × B → A
proj₁ (x , y) = x

proj₂ : ∀ {A B} → A × B → B
proj₂ (x , y) = y

-- not a strict substring
data SubString : List Char → Set where
  [] : SubString []
  _∷_  : ∀ {xs} x → SubString xs → SubString (x ∷ xs)
  skip : ∀ {xs x} → SubString xs → SubString (x ∷ xs)

forget : {xs : List Char} → SubString xs → List Char
forget [] = []
forget (x ∷ xs) = x ∷ forget xs
forget (skip xs) = forget xs

remember : (xs : List Char) → SubString xs
remember [] = []
remember (x ∷ xs) = x ∷ remember xs

_^_ : ℕ → ℕ → ℕ
n ^ zero = 1
n ^ (suc m) = n * (n ^ (m))

data Decimal : ℕ → ℕ → Set where
  [_] : (n : ℕ) → Decimal 1 n
  _∷_ : ∀ {place val} → (n : ℕ) → Decimal place val → Decimal (suc place) (n * 10 ^ place + val)

decimal : ∀ {place val} → Decimal place val → ℕ
decimal {_} {val} _ = val

fold-Decimal : ∀ {n} → Vec ℕ (suc n) → ℕ
fold-Decimal {zero} (x ∷ []) = x
fold-Decimal {suc n} (x ∷ xs) = x * 10 ^ (suc n) + fold-Decimal xs

to-Decimal : ∀ {n} → (xs : Vec ℕ (suc n)) → Decimal (suc n) (fold-Decimal xs)
to-Decimal {zero} (x ∷ []) = [ x ]
to-Decimal {suc n} (x ∷ xs) = x ∷ to-Decimal xs

maybe-decimal : ∀ {n} → (xs : Vec ℕ n) → Maybe ℕ
maybe-decimal [] = nothing
maybe-decimal (x ∷ xs) = just (decimal (to-Decimal (x ∷ xs)))

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

data Dar : ℕ → Set where
  dar : (c : Char) → Dar (toNat c)
