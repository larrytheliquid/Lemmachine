module Parser where
open import Data.Unit
open import Data.Bool
open import Data.Nat
open import Data.Char
open import Data.Maybe
open import Data.Sum
open import Data.Product hiding (_×_)
open import Data.List
open import Spiky
open import Spike
open import Data

read : (u : U) → List Char → Maybe (El u × List Char)
read _ [] = nothing
read CHAR (x ∷ xs) = just (x , xs)
read NAT (x ∷ xs) with within? x (toNat '0') (toNat '9')
... | false = nothing
... | true = just ( (toNat x) ∸ (toNat '0') , xs)
read METHOD ('G' ∷ 'E' ∷ 'T' ∷ xs) = just (GET , xs)
read METHOD ('H' ∷ 'E' ∷ 'A' ∷ 'D' ∷ xs) = just (HEAD , xs)
read METHOD ('P' ∷ 'O' ∷ 'S' ∷ 'T' ∷ xs) = just (POST , xs)
read METHOD _ = nothing
read _ _ = {!!}

parse : (f : Format) → List Char → Maybe (⟦ f ⟧ × List Char)
parse Fail _ = nothing
parse End xs = just (tt , xs)
parse (Base u) xs = read u xs
-- TODO: actually parse towards any later match
parse (Somewhere f) xs = parse f xs
-- TODO: actually parse Many for a first stab
parse (Between n m f) xs = parse f xs
parse (Skip f₁ f₂) xs with parse f₁ xs
... | nothing = nothing
... | just (_ , ys) = parse f₂ ys
parse (Or f₁ f₂) xs with parse f₁ xs
... | just (a , ys) = just (inj₁ a , ys)
... | nothing with parse f₂ xs
... | just (a , ys) = just (inj₂ a , ys)
... | nothing = nothing
parse (And f₁ f₂) xs with parse f₁ xs
... | nothing = nothing
... | just (a , ys) with parse f₂ ys
... | nothing = nothing
... | just (b , zs) = just ((a , b) , zs)
parse (Use f₁ f₂) xs with parse f₁ xs
... | nothing = nothing
... | just (a , ys) with parse (f₂ a) ys
... | nothing = nothing
... | just (b , zs) = just ((a , b) , zs)
