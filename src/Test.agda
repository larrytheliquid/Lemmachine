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

raw-ordered-post = here-doc "
POST / HTTP/1.0\r
Content-Length: 14\r
Content-Type: text/plain\r
User-Agent: Lemmachine\r
\r
Split The Atom
"

parsed-ordered-post : Request-Parse
parsed-ordered-post = parse-request (toList raw-ordered-post)

raw-jumbled-post = here-doc "
POST / HTTP/1.0\r
Content-Length: 14\r
User-Agent: Lemmachine\r
Content-Type: text/plain\r
\r
Split The Atom
"

parsed-jumbled-post : Request-Parse
parsed-jumbled-post = parse-request (toList raw-jumbled-post)

raw-jumbalaya-post = here-doc "
POST / HTTP/1.0\r
User-Agent: Lemmachine\r
Content-Type: text/plain\r
Content-Length: 33\r
\r
jumbalaya & fried pickles, please
"

parsed-jumbalaya-post : Request-Parse
parsed-jumbalaya-post = parse-request (toList raw-jumbalaya-post)
