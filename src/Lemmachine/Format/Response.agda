module Lemmachine.Format.Response where
open import Data.Nat
open import Data.Maybe
open import Data.Product
open import Lemmachine.Data
open import Lemmachine.HTTP
open import Lemmachine.Format

Simple-Format =
  Slurp (Base CHAR)

Shared-Headers-Format : Format
Shared-Headers-Format =
  Required-Header Date >>-
  Optional-Header Pragma >>-
  Optional-Header Server >>-
  End

Location-Format : Method → Code → Format
Location-Format _ 300-Multiple-Choices  = Optional-Header Location
Location-Format _ 301-Moved-Permanently = Required-Header Location
Location-Format _ 302-Moved-Temporarily = Required-Header Location
Location-Format _ _                     = Optional-Header Location

WWW-Authenticate-Format : Code → Format
WWW-Authenticate-Format 401-Unauthorized = Required-Header WWW-Authenticate
WWW-Authenticate-Format _                = End

Remaining-Format : Format → Method → Code → Format
Remaining-Format x _ 304-Not-Modified =
  Optional-Header Allow >>-
  Optional-Header Expires >>-
  x >>- 
  Headers-End >>
  End
Remaining-Format x m c =
  Optional-Header Allow >>-
  Optional-Header Content-Encoding >>-
  Required-Header Content-Length >>= λ c-l →
  Required-Header Content-Type >>-
  Optional-Header Expires >>-
  Optional-Header Last-Modified >>-
  x >>-
  Headers-End >>
  f m c (proj₁ c-l) (proj₁ (proj₂ c-l))

  where

  f : Method → Code → (s : Single Content-Length) → Header-Value (proj s) → Format
  f _    204-No-Content            _           _    = End
  f HEAD _                         _           _    = End
  f _    201-Created               (single ._) zero = Fail
  f _    202-Accepted              (single ._) zero = Fail
  f _    300-Multiple-Choices      (single ._) zero = Fail
  f _    301-Moved-Permanently     (single ._) zero = Fail
  f _    302-Moved-Temporarily     (single ._) zero = Fail
  f _    400-Bad-Request           (single ._) zero = Fail
  f _    401-Unauthorized          (single ._) zero = Fail
  f _    404-Not-Found             (single ._) zero = Fail
  f _    500-Internal-Server-Error (single ._) zero = Fail
  f _    501-Not-Implemented       (single ._) zero = Fail
  f _    502-Bad-Gateway           (single ._) zero = Fail
  f _    503-Service-Unavailable   (single ._) zero = Fail
  f _    _                         (single ._) n    = Base (STR n)

Full-Format : Method → Format
Full-Format m =
  Base VERSION >>-
  SP >>
  Base CODE >>= λ c →
  POST-Required-For-Created m c >>
  SP >>
  Base REASON-PHRASE >>-
  CRLF >>
  Location-Format m c >>-
  WWW-Authenticate-Format c >>-
  Remaining-Format Shared-Headers-Format m c

  where

  POST-Required-For-Created : Method → Code → Format
  POST-Required-For-Created POST 201-Created = End
  POST-Required-For-Created _    201-Created = Fail
  POST-Required-For-Created _    _           = End

Response-Format : Maybe Method → Format
Response-Format nothing  = Simple-Format
Response-Format (just m) = Full-Format m
