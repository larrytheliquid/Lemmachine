module Spiky where
open import Data.Unit
open import Data.String
open import Data.Product hiding (map)
open import Data.List

infix 4 _∈_
infixr 5 _∷_

data All {A : Set}(P : A → Set) : List A → Set where
  [] : All P []
  _∷_ : ∀ {x xs} → P x → All P xs → All P (x ∷ xs)

data _∈_ {A : Set}(x : A) : List A -> Set where 
  hd : ∀ {xs} → x ∈ x ∷ xs 
  tl : ∀ {y xs} → x ∈ xs → x ∈ y ∷ xs

data U : Set where
  Method Request-URI HTTP-Version : U
  message-body : U

El : U → Set
El _ = String

Request = List (Σ U El)

mutual
  data Valid : Request → Set where
    valid : ∀ {xs} → All (Validator xs) (map proj₁ xs) → Valid xs

  Validator : Request → U → Set
  Validator (x ∷ xs) Method = El (proj₁ x) × Valid xs
  Validator (x ∷ xs) message-body = ∃ λ y → y ∈ xs × Valid xs
  Validator (x ∷ xs) Request-URI = ∃ λ y → Valid (y ∷ xs)
  Validator _ _ = ⊤

