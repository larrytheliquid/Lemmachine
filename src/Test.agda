module Test where
open import Data.Char
open import Data.String
open import Data.Maybe
open import Data.Product
open import Data.List
open import Parser

here-doc : String → String
here-doc str with toList str
... | [] = ""
... | _ ∷ xs = fromList (chomp xs)
  where
  chomp : List Char → List Char
  chomp [] = []
  chomp ('\n' ∷ []) = []
  chomp (x ∷ xs) = x ∷ chomp xs

raw-get = here-doc "
GET / HTTP/1.0\r
User-Agent: Lemmachine\r
\r\n
"

parsed-get : Request-Parse
parsed-get = parse-request (toList raw-get)

raw-head = here-doc "
HEAD / HTTP/1.0\r
User-Agent: Lemmachine\r
\r\n
"

parsed-head : Request-Parse
parsed-head = parse-request (toList raw-head)

raw-post = here-doc "
POST / HTTP/1.0\r
Content-Length: 14\r
Content-Type: text/plain\r
User-Agent: Lemmachine\r
\r
Split The Atom
"

parsed-post : Request-Parse
parsed-post = parse-request (toList raw-post)
