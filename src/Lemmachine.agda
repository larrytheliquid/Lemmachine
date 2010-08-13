module Lemmachine where
open import Data.Empty
open import Data.Unit
open import Data.String
open import Data.Sum
open import Data.Product

infixr 1 _∣_

data U : Set where
  Date Pragma : U

el : U → Set
el _ = String

mutual
  data Format : Set where
    Bad End : Format
    Base : U → Format
    Plus Skip : Format → Format → Format
    Read : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Bad ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base u ⟧ = el u
  ⟦ Plus f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ Read f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

_∣_ : U → U → Format
x ∣ y = Plus (Base x) (Base y)

General-Header = Date ∣ Pragma
