module Spiky where
open import Data.Unit
open import Data.String
open import Data.Product
open import Data.List

infix 4 _∈_
infixr 5 _∷_

data All {A : Set}(P : A → Set) : List A → Set where
  [] : All P []
  _∷_ : ∀ {x xs} → P x → All P xs → All P (x ∷ xs)

data _∈_ {A : Set}(x : A) : List A -> Set where 
  here : ∀ {xs} → x ∈ x ∷ xs 
  there : ∀ {y xs} → x ∈ xs → x ∈ y ∷ xs

data U : Set where
  Method Request-URI HTTP-Version : U
  message-body : U

El : U → Set
El _ = String

Header = (Σ U El)
Request = List Header

-- map-with-∈ : ∀ {B} (xs : List A) → (∀ {x} → x ∈ xs → B) → List B
-- map-with-∈ []       f = []
-- map-with-∈ (x ∷ xs) f = f (here S.refl) ∷ map-with-∈ xs (f ∘ there)

mutual
  data Valid : Request → Set where
    valid : ∀ {xs} → All (Validator xs) xs → Valid xs

  Validator : (r : Request) → Header → Set
  Validator (x ∷ xs) (Method , _) = El (proj₁ x) × Valid xs
  Validator (x ∷ xs) (message-body , _) = ∃ λ y → y ∈ xs × Valid xs
  Validator (x ∷ xs) (Request-URI , _) = ∃ λ y → Valid (y ∷ xs)
  Validator _ _ = ⊤
