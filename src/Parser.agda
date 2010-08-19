module Parser where
open import Data.Unit
open import Data.Bool
open import Data.Nat
open import Data.Char
open import Data.String
open import Data.Maybe
open import Data.Sum
open import Data.Product hiding (_×_)
open import Data.List hiding (_++_)
open import Relation.Binary.PropositionalEquality1
open import Spiky
open import Spike
open import Data

read-ℕ : Char → Maybe ℕ
read-ℕ x with within? x (toNat '0') (toNat '9')
... | false = nothing
... | true = just (toNat x ∸ toNat '0')

read-Value-String : {m : Method} → (h : Header m) → Value h ≡₁ String → List Char → Maybe (Value h × List Char)
read-Value-String h p [] = nothing
read-Value-String h p ('\r' ∷ '\n' ∷ xs) with Value h | p
... | ._ | refl = just ("" , '\r' ∷ '\n' ∷ xs)
read-Value-String h p (x ∷ xs) with read-Value-String h p xs
... | nothing = nothing
... | just (a , ys) with Value h | p
... | ._ | refl = just ( fromList [ x ] ++ a  , ys )

read-Value-ℕ : {m : Method} → (h : Header m) → Value h ≡₁ ℕ → List Char → Maybe (Value h × List Char)
read-Value-ℕ h p [] = nothing
read-Value-ℕ h p (x ∷ '\r' ∷ '\n' ∷ xs) with Value h | p
... | ._ | refl with read-ℕ x
... | nothing = nothing
... | just n = just (n , '\r' ∷ '\n' ∷ xs)
read-Value-ℕ h p (x ∷ xs) with read-Value-ℕ h p xs
... | nothing = nothing
... | just (n , ys) with read-ℕ x | Value h | p
... | nothing | _ | _ = nothing
-- TODO: use correct num
... | just m | ._ | refl = just ( m + n  , ys )

read : (u : U) → List Char → Maybe (El u × List Char)
read _ [] = nothing

read CHAR (x ∷ xs) = just (x , xs)

read NAT (x ∷ xs) with read-ℕ x
... | nothing = nothing
... | just n = just (n , xs)

read METHOD ('G' ∷ 'E' ∷ 'T' ∷ xs) = just (GET , xs)
read METHOD ('H' ∷ 'E' ∷ 'A' ∷ 'D' ∷ xs) = just (HEAD , xs)
read METHOD ('P' ∷ 'O' ∷ 'S' ∷ 'T' ∷ xs) = just (POST , xs)
read METHOD _ = nothing

read (HEADER GET) ('P' ∷ 'r' ∷ 'a' ∷ 'g' ∷ 'm' ∷ 'a' ∷ xs) = just (Pragma , xs)
read (HEADER GET) ('A' ∷ 'u' ∷ 't' ∷ 'h' ∷ 'o' ∷ 'r' ∷ 'i' ∷ 'z' ∷ 'a' ∷ 't' ∷ 'i' ∷ 'o' ∷ 'n' ∷ xs) = just (Authorization , xs)
read (HEADER GET) ('F' ∷ 'r' ∷ 'o' ∷ 'm' ∷ xs) = just (From , xs)
read (HEADER GET) ('I' ∷ 'f' ∷ '-' ∷ 'M' ∷ 'o' ∷ 'd' ∷ 'i' ∷ 'f' ∷ 'i' ∷ 'e' ∷ 'd' ∷ '-' ∷ 'S' ∷ 'i' ∷ 'n' ∷ 'c' ∷ 'e' ∷ xs) = just (If-Modified-Since , xs)
read (HEADER GET) ('R' ∷ 'e' ∷ 'f' ∷ 'e' ∷ 'r' ∷ 'e' ∷ 'r' ∷ xs) = just (Referer , xs)
read (HEADER GET) ('U' ∷ 's' ∷ 'e' ∷ 'r' ∷ '-' ∷ 'A' ∷ 'g' ∷ 'e' ∷ 'n' ∷ 't' ∷ xs) = just (User-Agent , xs)
read (HEADER GET) _ = nothing

read (HEADER HEAD) ('P' ∷ 'r' ∷ 'a' ∷ 'g' ∷ 'm' ∷ 'a' ∷ xs) = just (Pragma , xs)
read (HEADER HEAD) ('A' ∷ 'u' ∷ 't' ∷ 'h' ∷ 'o' ∷ 'r' ∷ 'i' ∷ 'z' ∷ 'a' ∷ 't' ∷ 'i' ∷ 'o' ∷ 'n' ∷ xs) = just (Authorization , xs)
read (HEADER HEAD) ('F' ∷ 'r' ∷ 'o' ∷ 'm' ∷ xs) = just (From , xs)
read (HEADER HEAD) ('R' ∷ 'e' ∷ 'f' ∷ 'e' ∷ 'r' ∷ 'e' ∷ 'r' ∷ xs) = just (Referer , xs)
read (HEADER HEAD) ('U' ∷ 's' ∷ 'e' ∷ 'r' ∷ '-' ∷ 'A' ∷ 'g' ∷ 'e' ∷ 'n' ∷ 't' ∷ xs) = just (User-Agent , xs)
read (HEADER HEAD) _ = nothing

read (HEADER POST) ('D' ∷ 'a' ∷ 't' ∷ 'e' ∷ xs) = just (Date , xs)
read (HEADER POST) ('P' ∷ 'r' ∷ 'a' ∷ 'g' ∷ 'm' ∷ 'a' ∷ xs) = just (Pragma , xs)
read (HEADER POST) ('C' ∷ 'o' ∷ 'n' ∷ 't' ∷ 'e' ∷ 'n' ∷ 't' ∷ '-' ∷ 'E' ∷ 'n' ∷ 'c' ∷ 'o' ∷ 'd' ∷ 'i' ∷ 'n' ∷ 'g' ∷ xs) = just (Content-Encoding , xs)
read (HEADER POST) ('C' ∷ 'o' ∷ 'n' ∷ 't' ∷ 'e' ∷ 'n' ∷ 't' ∷ '-' ∷ 'L' ∷ 'e' ∷ 'n' ∷ 'g' ∷ 't' ∷ 'h' ∷ xs) = just (Content-Length , xs)
read (HEADER POST) ('C' ∷ 'o' ∷ 'n' ∷ 't' ∷ 'e' ∷ 'n' ∷ 't' ∷ '-' ∷ 'T' ∷ 'y' ∷ 'p' ∷ 'e' ∷ xs) = just (Content-Type , xs)
read (HEADER POST) _ = nothing

read (VALUE {GET} Pragma) xs            = read-Value-String {GET} Pragma refl xs
read (VALUE {GET} Authorization) xs     = read-Value-String {GET} Authorization refl xs
read (VALUE {GET} From) xs              = read-Value-String {GET} From refl xs
read (VALUE {GET} If-Modified-Since) xs = read-Value-String {GET} If-Modified-Since refl xs
read (VALUE {GET} Referer) xs           = read-Value-String {GET} Referer refl xs
read (VALUE {GET} User-Agent) xs        = read-Value-String {GET} User-Agent refl xs

read (VALUE {HEAD} Pragma) xs        = read-Value-String {HEAD} Pragma refl xs
read (VALUE {HEAD} Authorization) xs = read-Value-String {HEAD} Authorization refl xs
read (VALUE {HEAD} From) xs          = read-Value-String {HEAD} From refl xs
read (VALUE {HEAD} Referer) xs       = read-Value-String {HEAD} Referer refl xs
read (VALUE {HEAD} User-Agent) xs    = read-Value-String {HEAD} User-Agent refl xs

read (VALUE {POST} Date) xs             = read-Value-String {POST} Date refl xs
read (VALUE {POST} Pragma) xs           = read-Value-String {POST} Pragma refl xs
read (VALUE {POST} Content-Encoding) xs = read-Value-String {POST} Content-Encoding refl xs
read (VALUE {POST} Content-Length) xs   = read-Value-ℕ {POST} Content-Length refl xs
read (VALUE {POST} Content-Type) xs     = read-Value-String {POST} Content-Type refl xs

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
