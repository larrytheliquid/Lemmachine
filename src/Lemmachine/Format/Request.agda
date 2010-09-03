module Lemmachine.Format.Request where
open import Data.Product
open import Lemmachine.Data hiding ([_])
open import Lemmachine.HTTP
open import Lemmachine.Format

Simple-Request-Format =
  str "GET" >>
  SP >>
  Base REQUEST-URI >>-
  CRLF >>
  End

GET-Format : Format
GET-Format =
  Slurp (
    Base HEADER-NAME >>= λ h →
    char ':' >>
    SP >>
    Base (HEADER-VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

HEAD-Format = GET-Format

POST-Format : Format
POST-Format =
  Required-Header Content-Length >>= λ c-l →
  f (proj₁ c-l) (proj₁ (proj₂ c-l))

  where

  f : (s : Single Content-Length) → Header-Value (proj s) → Format
  f (single ._) n =
    Required-Header Content-Type >>-

    Slurp (
      Base HEADER-NAME >>= λ h →
      char ':' >>
      SP >>
      Base (HEADER-VALUE h) >>-
      CRLF >>
      End
    ) >>-

    CRLF >>
    Base (STR n)

Remaining-Format : Method → Format
Remaining-Format GET  = GET-Format
Remaining-Format HEAD = HEAD-Format
Remaining-Format POST = POST-Format

Full-Request-Format =
  Base METHOD >>= λ m →
  SP >>
  Base REQUEST-URI >>-
  SP >>
  Base VERSION >>-
  CRLF >>  
  Remaining-Format m

Request-Format =
  Full-Request-Format ∣ Simple-Request-Format

