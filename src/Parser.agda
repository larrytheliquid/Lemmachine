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

read-Digit : Char → Maybe ℕ
read-Digit x with within? x (toNat '0') (toNat '9')
... | false = nothing
... | true = just (toNat x ∸ toNat '0')

read-Digits : List Char → Maybe (List ℕ × List Char)
read-Digits [] = nothing
read-Digits (x ∷ xs) with read-Digit x
... | nothing = nothing
... | just n with read-Digits xs
... | nothing = just (n ∷ [] , xs)
... | just (ns , ys) = just (n ∷ ns , ys)

read-Decimal : List Char → Maybe (ℕ × List Char)
read-Decimal xs with read-Digits xs
... | nothing = nothing
... | just (ns , ys) with maybe-decimal (Data.Vec.fromList ns)
... | nothing = nothing
... | just n = just (n , ys)

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

read : (u : U) → List Char → Maybe (El u × List Char)
read CHAR (x ∷ xs) = just (x , xs)

read NAT xs = read-Decimal xs

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

read REASON-PHRASE xs = read-to-CRLF xs

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

read (VALUE {GET} Pragma) xs            = read-to-CRLF xs
read (VALUE {GET} Authorization) xs     = read-to-CRLF xs
read (VALUE {GET} From) xs              = read-to-CRLF xs
read (VALUE {GET} If-Modified-Since) xs = read-to-CRLF xs
read (VALUE {GET} Referer) xs           = read-to-CRLF xs
read (VALUE {GET} User-Agent) xs        = read-to-CRLF xs

read (VALUE {HEAD} Pragma) xs        = read-to-CRLF xs
read (VALUE {HEAD} Authorization) xs = read-to-CRLF xs
read (VALUE {HEAD} From) xs          = read-to-CRLF xs
read (VALUE {HEAD} Referer) xs       = read-to-CRLF xs
read (VALUE {HEAD} User-Agent) xs    = read-to-CRLF xs

read (VALUE {POST} Date) xs             = read-to-CRLF xs
read (VALUE {POST} Pragma) xs           = read-to-CRLF xs
read (VALUE {POST} Authorization) xs    = read-to-CRLF xs
read (VALUE {POST} From) xs             = read-to-CRLF xs
read (VALUE {POST} Referer) xs          = read-to-CRLF xs
read (VALUE {POST} User-Agent) xs       = read-to-CRLF xs
read (VALUE {POST} Content-Encoding) xs = read-to-CRLF xs
read (VALUE {POST} Content-Length) xs   = read-Decimal xs
read (VALUE {POST} Content-Type) xs     = read-to-CRLF xs

read _ [] = nothing

parse : (f : Format) → List Char → Maybe (⟦ f ⟧ × List Char)
parse Fail _ = nothing
parse End xs = just (tt , xs)
parse (Base u) xs = read u xs
parse (Upto _ _) [] = nothing
parse (Upto end f) (x ∷ xs) with parse end (x ∷ xs) | parse f (x ∷ xs) | parse (Upto end f) xs
... | nothing | just ans | _             = just ans
... | nothing | nothing  | just (a , ys) = just (a , x ∷ ys)
... | _       | _        | _             = nothing
parse (Slurp f) [] = just ([] , [])
parse (Slurp f) (x ∷ xs) with parse f (x ∷ xs) | parse (Slurp f) xs
... | _ | nothing = nothing -- never happens
... | nothing | just ([] , zs) = just ([] , x ∷ zs)
... | nothing | just (as , zs) = just (as , zs)
... | just (a , ys) | just ([] , _)  = just (a ∷ [] , ys)
... | just (a , _)  | just (as , zs) = just (a ∷ as , zs)
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

Response-Parse = Maybe (⟦ Response-Format ⟧ × List Char)

parse-response : List Char → Response-Parse
parse-response xs = parse Response-Format xs
