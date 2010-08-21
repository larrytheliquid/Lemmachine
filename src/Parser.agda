module Parser where
open import Data.Unit
open import Data.Bool
open import Data.Nat
open import Data.Char
open import Data.String
open import Data.Maybe
open import Data.Sum
open import Data.Product hiding (_×_)
open import Data.List hiding (_++_; [_])
open import Data.Vec hiding (_++_; fromList)
open import Relation.Nullary
open import Relation.Binary.PropositionalEquality1
open import Spiky
open import Spike
open import Data

read-ℕ : Char → Maybe ℕ
read-ℕ x with within? x (toNat '0') (toNat '9')
... | false = nothing
... | true = just (toNat x ∸ toNat '0')

read-to-SP : List Char → Maybe (String × List Char)
read-to-SP [] = nothing
read-to-SP (' ' ∷ xs) = just ("" , ' ' ∷ xs)
read-to-SP (x ∷ xs) with read-to-SP xs
... | nothing = nothing
... | just (a , ys) = just ( fromList (x ∷ []) ++ a  , ys )

read-to-CRLF : List Char → Maybe (String × List Char)
read-to-CRLF [] = nothing
read-to-CRLF ('\r' ∷ '\n' ∷ xs) = just ("" , '\r' ∷ '\n' ∷ xs)
read-to-CRLF (x ∷ xs) with read-to-CRLF xs
... | nothing = nothing
... | just (a , ys) = just ( fromList (x ∷ []) ++ a  , ys )

read-Value-String : {m : Method} → (h : Header m) → Value h ≡₁ String → List Char → Maybe (Value h × List Char)
read-Value-String h p xs with Value h | p
... | ._ | refl = read-to-CRLF xs

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
read CHAR (x ∷ xs) = just (x , xs)

read NAT (x ∷ xs) with read-ℕ x
... | nothing = nothing
... | just n = just (n , xs)

read (DAR n) (x ∷ xs) with Data.Nat._≟_ n (toNat x)
... | no _  = nothing
... | yes p rewrite p = just (dar x , xs)

read (DAR-RANGE n m) (x ∷ xs) with Data.Bool._≟_ true (within? x n m)
... | no _ = nothing
... | yes p rewrite p = just (dar x , xs)

read (SINGLE (HEADER POST) y) (x ∷ xs) with read (HEADER POST) (x ∷ xs)
... | nothing = nothing
read (SINGLE (HEADER POST) Content-Length) (_ ∷ _) | just (Content-Length , ys) = just (single Content-Length , ys)
read (SINGLE (HEADER POST) Content-Type) (_ ∷ _)   | just (Content-Type , ys)   = just (single Content-Type , ys)
... | just _ = nothing
-- TODO: Single via Decidable equality on other values + types
read (SINGLE _ _) (x ∷ xs) = nothing

read (STR zero) [] = just ([] , [])
read (STR zero) (x ∷ xs) = just ([] , (x ∷ xs))
read (STR (suc n)) (x ∷ xs) with read (STR n) xs
... | nothing = nothing
... | just (str , ys) = just (x ∷ str , ys)

read METHOD ('G' ∷ 'E' ∷ 'T' ∷ xs)       = just (GET , xs)
read METHOD ('H' ∷ 'E' ∷ 'A' ∷ 'D' ∷ xs) = just (HEAD , xs)
read METHOD ('P' ∷ 'O' ∷ 'S' ∷ 'T' ∷ xs) = just (POST , xs)
read METHOD _ = nothing

read REQUEST-URI ('/' ∷ xs) = read-to-SP ('/' ∷ xs)
read REQUEST-URI (_ ∷ _) = nothing

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
read (HEADER POST) ('A' ∷ 'u' ∷ 't' ∷ 'h' ∷ 'o' ∷ 'r' ∷ 'i' ∷ 'z' ∷ 'a' ∷ 't' ∷ 'i' ∷ 'o' ∷ 'n' ∷ xs) = just (Authorization , xs)
read (HEADER POST) ('F' ∷ 'r' ∷ 'o' ∷ 'm' ∷ xs) = just (From , xs)
read (HEADER POST) ('R' ∷ 'e' ∷ 'f' ∷ 'e' ∷ 'r' ∷ 'e' ∷ 'r' ∷ xs) = just (Referer , xs)
read (HEADER POST) ('U' ∷ 's' ∷ 'e' ∷ 'r' ∷ '-' ∷ 'A' ∷ 'g' ∷ 'e' ∷ 'n' ∷ 't' ∷ xs) = just (User-Agent , xs)
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
read (VALUE {POST} Authorization) xs    = read-Value-String {POST} Authorization refl xs
read (VALUE {POST} From) xs             = read-Value-String {POST} From refl xs
read (VALUE {POST} Referer) xs          = read-Value-String {POST} Referer refl xs
read (VALUE {POST} User-Agent) xs       = read-Value-String {POST} User-Agent refl xs
read (VALUE {POST} Content-Encoding) xs = read-Value-String {POST} Content-Encoding refl xs
read (VALUE {POST} Content-Length) xs   = read-Value-ℕ {POST} Content-Length refl xs
read (VALUE {POST} Content-Type) xs     = read-Value-String {POST} Content-Type refl xs

read _ [] = nothing

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

Request-Parse = Maybe (⟦ Request-Format ⟧ × List Char)

parse-request : List Char → Request-Parse
parse-request xs = parse Request-Format xs
