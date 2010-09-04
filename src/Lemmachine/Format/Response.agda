module Lemmachine.Format.Response where
open import Data.Nat
open import Data.Maybe
open import Data.Product
open import Lemmachine.Data hiding ([_])
open import Lemmachine.HTTP
open import Lemmachine.Format
open import Lemmachine.Format.Request

Simple-Response-Format =
  Slurp (Base CHAR)

Shared-Response-Headers-Format : Format
Shared-Response-Headers-Format =
  Required-Header Date >>-
  Optional-Header Pragma >>-
  Optional-Header Server >>-
  End

HEAD-Response-Format : Format
HEAD-Response-Format =
  Shared-Response-Headers-Format

GET-Response-Format = HEAD-Response-Format
POST-Response-Format =
  Shared-Response-Headers-Format

Method-Response-Format : Method → Format
Method-Response-Format GET  = GET-Response-Format
Method-Response-Format HEAD = HEAD-Response-Format
Method-Response-Format POST = POST-Response-Format

Location-Format : Method → Code → Format
Location-Format _    300-Multiple-Choices  = Optional-Header Location
Location-Format _    301-Moved-Permanently = Required-Header Location
Location-Format _    302-Moved-Temporarily = Required-Header Location
Location-Format _    _                     = Optional-Header Location

WWW-Authenticate-Format : Code → Format
WWW-Authenticate-Format 401-Unauthorized = Required-Header WWW-Authenticate
WWW-Authenticate-Format _                = End

Entity-Body-Format : Format → Method → Code → Format
Entity-Body-Format x _    204-No-Content   = x >>- CRLF >> End
Entity-Body-Format x _    304-Not-Modified = x >>- CRLF >> End
Entity-Body-Format x m c =
  Optional-Header Allow >>-
  Optional-Header Content-Encoding >>-
  Required-Header Content-Length >>= λ c-l →
  Required-Header Content-Type >>-
  Optional-Header Expires >>-
  Optional-Header Last-Modified >>-
  x >>-
  CRLF >>
  f m c (proj₁ c-l) (proj₁ (proj₂ c-l))
  where
  f : Method → Code → (s : Single Content-Length) → Header-Value (proj s) → Format
  f HEAD _                    _           _    = End
  f _    300-Multiple-Choices  (single ._) zero    = Fail
  f _    301-Moved-Permanently (single ._) zero    = Fail
  f _    302-Moved-Temporarily (single ._) zero    = Fail
  f _    304-Not-Modified      (single ._) (suc _) = Fail
  f _    _                     (single ._) n       = Base (STR n)

Full-Response-Format : Method → Format
Full-Response-Format m =
  Base VERSION >>-
  SP >>
  Base CODE >>= λ c →
  Require-POST-If-Created m c >>
  SP >>
  Base REASON-PHRASE >>-
  CRLF >>
  Location-Format m c >>-
  WWW-Authenticate-Format c >>-
  Entity-Body-Format (Method-Response-Format m) m c

  where

  Require-POST-If-Created : Method → Code → Format
  Require-POST-If-Created GET  201-Created = Fail
  Require-POST-If-Created HEAD 201-Created = Fail
  Require-POST-If-Created _    _           = End

Response-Format : Maybe Method → Format
Response-Format nothing  = Simple-Response-Format
Response-Format (just m) = Full-Response-Format m
