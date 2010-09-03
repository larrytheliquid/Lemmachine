module Lemmachine.Test where
open import Data.Char
open import Data.String
open import Data.Maybe
open import Data.Product
open import Data.List
open import Lemmachine

here-doc : String → String
here-doc str with toList str
... | [] = ""
... | _ ∷ xs = fromList (chomp xs)
  where
  chomp : List Char → List Char
  chomp [] = []
  chomp ('\n' ∷ []) = []
  chomp (x ∷ xs) = x ∷ chomp xs

raw-simple-request = here-doc "
GET /foo\r\n
"

parsed-simple-request : Request-Parse
parsed-simple-request = parse-request (toList raw-simple-request)

raw-simple-response = here-doc "
bar!
"

parsed-simple-response : Response-Parse parsed-simple-request
parsed-simple-response = parse-response 
  parsed-simple-request 
  (toList raw-simple-response)

raw-get-request = here-doc "
GET / HTTP/1.0\r
From: larrytheliquid@gmail.com\r
User-Agent: Lemmachine\r
\r\n
"

parsed-get-request : Request-Parse
parsed-get-request = parse-request (toList raw-get-request)

raw-get-response = here-doc "
HTTP/1.0 200 OK\r
Date: Fri, 03 Sep 2010 19:33:51 GMT\r
Server: Lemmachine\r
Content-Length: 31\r
Content-Type: text/plain\r
\r
Great success, I very exciiite!
"

parsed-get-response : Response-Parse parsed-get-request
parsed-get-response = parse-response
  parsed-get-request
  (toList raw-get-response)

raw-headerless = here-doc "
GET / HTTP/1.0\r
\r\n
"

parsed-headerless : Request-Parse
parsed-headerless = parse-request (toList raw-headerless)

raw-head = here-doc "
HEAD / HTTP/1.0\r
From: larrytheliquid@gmail.com\r
User-Agent: Lemmachine\r
\r\n
"

parsed-head : Request-Parse
parsed-head = parse-request (toList raw-head)

raw-ordered-post = here-doc "
POST / HTTP/1.0\r
Content-Length: 14\r
Content-Type: text/plain\r
From: larrytheliquid@gmail.com\r
User-Agent: Lemmachine\r
\r
Split The Atom
"

parsed-ordered-post : Request-Parse
parsed-ordered-post = parse-request (toList raw-ordered-post)

raw-jumbled-post = here-doc "
POST / HTTP/1.0\r
Content-Length: 14\r
From: larrytheliquid@gmail.com\r
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
From: larrytheliquid@gmail.com\r
Content-Length: 33\r
\r
jumbalaya & fried pickles, please
"

parsed-jumbalaya-post : Request-Parse
parsed-jumbalaya-post = parse-request (toList raw-jumbalaya-post)
