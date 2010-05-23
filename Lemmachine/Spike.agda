module Lemmachine.Spike where
open import Data.Fin
open import Data.Digit

-- 3.9 Quality Values

DIGIT = Decimal

data qvalue : DIGIT → DIGIT → DIGIT → Set where
  zero : (d₁ d₂ d₃ : DIGIT) → qvalue d₁ d₂ d₃
  one : qvalue zero zero zero
