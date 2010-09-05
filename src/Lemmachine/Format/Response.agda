module Lemmachine.Format.Response where
open import Data.Bool
open import Data.Nat
open import Data.Maybe
open import Data.Sum
open import Data.Product
open import Lemmachine.Data
open import Lemmachine.HTTP
open import Lemmachine.Format

Simple-Format =
  Slurp (Base CHAR)

Location-Format : Method → Code → Format
Location-Format _ 300-Multiple-Choices  = Optional-Header Location
Location-Format _ 301-Moved-Permanently = Required-Header Location
Location-Format _ 302-Moved-Temporarily = Required-Header Location
Location-Format _ _                     = Optional-Header Location

WWW-Authenticate-Format : Code → Format
WWW-Authenticate-Format 401-Unauthorized = Required-Header WWW-Authenticate
WWW-Authenticate-Format _                = End

Last-Modified-Format : ⟦ Required-Header Date ⟧ → Format
Last-Modified-Format (single ._ , date , tt) =
  Optional-Header Last-Modified >>= λ l-m →
  f l-m date

  where

  -- TODO: actual date comparison
  _>?_ : Header-Value Last-Modified → Header-Value Date → Bool
  x >? y = false

  guard : (s : Single Last-Modified) → Header-Value (proj s) → Header-Value Date → Format
  guard (single ._) l-m d with l-m >? d
  ... | true  = Fail
  ... | false = End

  f : ⟦ Optional-Header Last-Modified ⟧ → Header-Value Date → Format
  f (inj₁ l-m) date = guard (proj₁ l-m) (proj₁ (proj₂ l-m)) date
  f (inj₂ tt)  _    = End

Remaining-Format : ⟦ Required-Header Date ⟧ → Method → Code → Format
Remaining-Format _ _ 304-Not-Modified =
  Optional-Header Allow >>-
  Optional-Header Expires >>-
  Headers-End >>
  End
Remaining-Format d m c =
  Optional-Header Allow >>-
  Optional-Header Content-Encoding >>-
  Required-Header Content-Length >>= λ c-l →
  Required-Header Content-Type >>-
  Optional-Header Expires >>-
  Last-Modified-Format d >>-
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

  Required-Header Date >>= λ d →
  Optional-Header Pragma >>-
  Optional-Header Server >>-
  Location-Format m c >>-
  WWW-Authenticate-Format c >>-
  Remaining-Format d m c

  where

  POST-Required-For-Created : Method → Code → Format
  POST-Required-For-Created POST 201-Created = End
  POST-Required-For-Created _    201-Created = Fail
  POST-Required-For-Created _    _           = End

Response-Format : Maybe Method → Format
Response-Format nothing  = Simple-Format
Response-Format (just m) = Full-Format m
