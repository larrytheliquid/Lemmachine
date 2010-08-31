module Parser where
open import Data.Unit
open import Data.Bool
open import Data.Nat
open import Data.Char
open import Data.String
open import Data.Maybe
open import Data.Sum
open import Data.Product
open import Data.List hiding (_++_; [_])
open import Data.Vec hiding (_++_; fromList)
open import Relation.Nullary
open import Relation.Binary.PropositionalEquality1
open import Format
open import HTTP
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

read-Version : List Char → Maybe (Version × List Char)
read-Version ('H' ∷ 'T' ∷ 'T' ∷ 'P' ∷ '/' ∷ '1' ∷ '.' ∷ '0' ∷ xs) = just (HTTP/1∶0 , xs)
read-Version _ = nothing

read-Method : List Char → Maybe (Method × List Char)
read-Method ('G' ∷ 'E' ∷ 'T' ∷ xs) = just (GET , xs)
read-Method ('H' ∷ 'E' ∷ 'A' ∷ 'D' ∷ xs) = just (HEAD , xs)
read-Method ('P' ∷ 'O' ∷ 'S' ∷ 'T' ∷ xs) = just (POST , xs)
read-Method _ = nothing

read-Code : List Char → Maybe (Code × List Char)
read-Code (x ∷ y ∷ z ∷ xs) with read-Digit x | read-Digit y | read-Digit z
... | nothing | _       | _ = nothing
... | _       | nothing | _ = nothing
... | _       | _       | nothing = nothing
... | just n₁ | just n₂ | just n₃ with n₁ | n₂ | n₃
... | 2 | 0 | 0 = just (200-OK , xs)
... | 2 | 0 | 1 = just (201-Created , xs)
... | 2 | 0 | 2 = just (202-Accepted , xs)
... | 2 | _ | _ = just (200-OK , xs)
... | 3 | 0 | 0 = just (300-Multiple-Choices , xs)
... | 3 | 0 | 1 = just (301-Moved-Permanently , xs)
... | 3 | 0 | 2 = just (302-Moved-Temporarily , xs)
... | 3 | 0 | 4 = just (304-Not-Modified , xs)
... | 3 | _ | _ = just (300-Multiple-Choices , xs)
... | 4 | 0 | 0 = just (400-Bad-Request , xs)
... | 4 | 0 | 1 = just (401-Unauthorized , xs)
... | 4 | 0 | 3 = just (403-Forbidden , xs)
... | 4 | 0 | 4 = just (404-Not-Found , xs)
... | 4 | _ | _ = just (400-Bad-Request , xs)
... | 5 | 0 | 0 = just (500-Internal-Server-Error , xs)
... | 5 | 0 | 1 = just (501-Not-Implemented , xs)
... | 5 | 0 | 2 = just (502-Bad-Gateway , xs)
... | 5 | 0 | 3 = just (503-Service-Unavailable , xs)
... | 5 | _ | _ = just (500-Internal-Server-Error , xs)
... | _ | _ | _ = nothing

read-Code _ = nothing

read-Header-Name : List Char → Maybe (Header-Name × List Char)
read-Header-Name ('D' ∷ 'a' ∷ 't' ∷ 'e' ∷ xs) = just (Date , xs)
read-Header-Name ('P' ∷ 'r' ∷ 'a' ∷ 'g' ∷ 'm' ∷ 'a' ∷ xs) = just (Pragma , xs)
read-Header-Name ('A' ∷ 'u' ∷ 't' ∷ 'h' ∷ 'o' ∷ 'r' ∷ 'i' ∷ 'z' ∷ 'a' ∷ 't' ∷ 'i' ∷ 'o' ∷ 'n' ∷ xs) = just (Authorization , xs)
read-Header-Name ('F' ∷ 'r' ∷ 'o' ∷ 'm' ∷ xs) = just (From , xs)
read-Header-Name ('I' ∷ 'f' ∷ '-' ∷ 'M' ∷ 'o' ∷ 'd' ∷ 'i' ∷ 'f' ∷ 'i' ∷ 'e' ∷ 'd' ∷ '-' ∷ 'S' ∷ 'i' ∷ 'n' ∷ 'c' ∷ 'e' ∷ xs) = just (If-Modified-Since , xs)
read-Header-Name ('R' ∷ 'e' ∷ 'f' ∷ 'e' ∷ 'r' ∷ 'e' ∷ 'r' ∷ xs) = just (Referer , xs)
read-Header-Name ('U' ∷ 's' ∷ 'e' ∷ 'r' ∷ '-' ∷ 'A' ∷ 'g' ∷ 'e' ∷ 'n' ∷ 't' ∷ xs) = just (User-Agent , xs)
read-Header-Name ('L' ∷ 'o' ∷ 'c' ∷ 'a' ∷ 't' ∷ 'i' ∷ 'o' ∷ 'n' ∷ xs) = just (Location , xs)
read-Header-Name ('S' ∷ 'e' ∷ 'r' ∷ 'v' ∷ 'e' ∷ 'r' ∷ xs) = just (Server , xs)
read-Header-Name ('W' ∷ 'W' ∷ 'W' ∷ '-' ∷ 'A' ∷ 'u' ∷ 't' ∷ 'h' ∷ 'e' ∷ 'n' ∷ 't' ∷ 'i' ∷ 'c' ∷ 'a' ∷ 't' ∷ 'e' ∷ xs) = just (WWW-Authenticate , xs)
read-Header-Name ('A' ∷ 'l' ∷ 'l' ∷ 'o' ∷ 'w' ∷ xs) = just (Allow , xs)
read-Header-Name ('C' ∷ 'o' ∷ 'n' ∷ 't' ∷ 'e' ∷ 'n' ∷ 't' ∷ '-' ∷ 'E' ∷ 'n' ∷ 'c' ∷ 'o' ∷ 'd' ∷ 'i' ∷ 'n' ∷ 'g' ∷ xs) = just (Content-Encoding , xs)
read-Header-Name ('C' ∷ 'o' ∷ 'n' ∷ 't' ∷ 'e' ∷ 'n' ∷ 't' ∷ '-' ∷ 'L' ∷ 'e' ∷ 'n' ∷ 'g' ∷ 't' ∷ 'h' ∷ xs) = just (Content-Length , xs)
read-Header-Name ('C' ∷ 'o' ∷ 'n' ∷ 't' ∷ 'e' ∷ 'n' ∷ 't' ∷ '-' ∷ 'T' ∷ 'y' ∷ 'p' ∷ 'e' ∷ xs) = just (Content-Type , xs)
read-Header-Name ('E' ∷ 'x' ∷ 'p' ∷ 'i' ∷ 'r' ∷ 'e' ∷ 's' ∷ xs) = just (Expires , xs)
read-Header-Name ('L' ∷ 'a' ∷ 's' ∷ 't' ∷ '-' ∷ 'M' ∷ 'o' ∷ 'd' ∷ 'i' ∷ 'f' ∷ 'i' ∷ 'e' ∷ 'd' ∷ xs) = just (Last-Modified , xs)
read-Header-Name _  = nothing

read : (u : U) → List Char → Maybe (El u × List Char)
read CHAR (x ∷ xs) = just (x , xs)

read NAT xs = read-Decimal xs

read (DAR n) (x ∷ xs) with Data.Nat._≟_ n (toNat x)
... | no _  = nothing
... | yes p rewrite p = just (dar x , xs)

read (DAR-RANGE n m) (x ∷ xs) with Data.Bool._≟_ true (within? x n m)
... | no _ = nothing
... | yes p rewrite p = just (dar x , xs)

read (SINGLE h-n) (x ∷ xs) with read-Header-Name (x ∷ xs)
... | nothing = nothing
... | just (h-n₂ , ys) with h-n m≟ h-n₂
... | just (yes p) rewrite p = just (single h-n₂ , ys)
... | just (no _) = nothing
... | nothing = nothing

read (STR zero) [] = just ([] , [])
read (STR zero) (x ∷ xs) = just ([] , (x ∷ xs))
read (STR (suc n)) (x ∷ xs) with read (STR n) xs
... | nothing = nothing
... | just (str , ys) = just (x ∷ str , ys)

read VERSION xs = read-Version xs
read METHOD xs = read-Method xs
read CODE   xs = read-Code xs

read REQUEST-URI ('/' ∷ xs) = read-to-SP ('/' ∷ xs)
read REQUEST-URI (_ ∷ _) = nothing

read REASON-PHRASE xs = read-to-CRLF xs

read HEADER-NAME xs  = read-Header-Name xs

read (HEADER-VALUE Date) xs = read-to-CRLF xs
read (HEADER-VALUE Pragma) xs = read-to-CRLF xs
read (HEADER-VALUE Authorization) xs = read-to-CRLF xs
read (HEADER-VALUE From) xs = read-to-CRLF xs
read (HEADER-VALUE If-Modified-Since) xs = read-to-CRLF xs
read (HEADER-VALUE Referer) xs = read-to-CRLF xs
read (HEADER-VALUE User-Agent) xs = read-to-CRLF xs
read (HEADER-VALUE Location) xs = read-to-CRLF xs
read (HEADER-VALUE Server) xs = read-to-CRLF xs
read (HEADER-VALUE WWW-Authenticate) xs = read-to-CRLF xs
read (HEADER-VALUE Allow) xs = read-to-CRLF xs
read (HEADER-VALUE Content-Encoding) xs = read-to-CRLF xs
read (HEADER-VALUE Content-Length) xs = read-Decimal xs
read (HEADER-VALUE Content-Type) xs = read-to-CRLF xs
read (HEADER-VALUE Expires) xs = read-to-CRLF xs
read (HEADER-VALUE Last-Modified) xs = read-to-CRLF xs

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

Response-Parse : Method → Set
Response-Parse m = Maybe (⟦ Response-Format m ⟧ × List Char)

parse-response : (m : Method) → List Char → Response-Parse m
parse-response m xs = parse (Response-Format m) xs
