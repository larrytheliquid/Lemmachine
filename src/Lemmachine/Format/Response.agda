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

GET-Response-Format : Format
GET-Response-Format =
  Required-Header Date >>-

  Slurp (
    Base HEADER-NAME >>= λ h →
    char ':' >>
    SP >>
    Base (HEADER-VALUE h) >>-
    CRLF >>
    End
  )

HEAD-Response-Format = GET-Response-Format
POST-Response-Format = GET-Response-Format

Method-Response-Format : Method → Format
Method-Response-Format GET  = GET-Response-Format
Method-Response-Format HEAD = HEAD-Response-Format
Method-Response-Format POST = POST-Response-Format

-- TODO: Properly comply with 3xx & 201 wrt optional/required
Location-Format : Method → Code → Format
Location-Format _    300-Multiple-Choices  = Required-Header Location
Location-Format _    301-Moved-Permanently = Required-Header Location
Location-Format _    302-Moved-Temporarily = Required-Header Location
Location-Format _    304-Not-Modified      = Required-Header Location
Location-Format POST 201-Created           = Required-Header Location
Location-Format _    _                     = End

WWW-Authenticate-Format : Code → Format
WWW-Authenticate-Format 401-Unauthorized = Required-Header WWW-Authenticate
WWW-Authenticate-Format _ = End

Entity-Body-Format : Format → Method → Code → Format
Entity-Body-Format body _    204-No-Content   = body >>- CRLF >> End
Entity-Body-Format body _    304-Not-Modified = body >>- CRLF >> End
Entity-Body-Format body HEAD _                = body >>- CRLF >> End
Entity-Body-Format body _    _ = -- GET/POST
  Required-Header Content-Length >>= λ c-l →
  f body (proj₁ c-l) (proj₁ (proj₂ c-l))

  where

  f : Format → (s : Single Content-Length) → Header-Value (proj s) → Format
  f body (single ._) zero = body >>- CRLF >> End
  f body (single ._) n =
    Required-Header Content-Type >>-
    body >>-
    CRLF >>
    Base (STR n)

Full-Response-Format : Method → Format
Full-Response-Format m =
  Base VERSION >>-
  SP >>
  Base CODE >>= λ c →
  guard m c >>
  SP >>
  Base REASON-PHRASE >>-
  CRLF >>
  Location-Format m c >>-
  WWW-Authenticate-Format c >>-
  Entity-Body-Format (Method-Response-Format m) m c

  where

  guard : Method → Code → Format
  guard GET  201-Created = Fail
  guard HEAD 201-Created = Fail
  guard _    _           = End

Response-Format : Maybe Method → Format
Response-Format nothing  = Simple-Response-Format
Response-Format (just m) = Full-Response-Format m
