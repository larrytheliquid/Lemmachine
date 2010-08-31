module Format where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.Char
open import Data.String
open import Data.Nat
open import Data.List hiding ([_])
open import Data.Vec hiding (_>>=_; toList; [_])
open import Data.Sum
open import Data.Product
open import Data hiding ([_])
open import HTTP

infixr 3 _∣_
infixr 1 _>>_ _>>-_ _>>=_

data U : Set where
  CHAR NAT : U
  DAR : ℕ → U
  DAR-RANGE : ℕ → ℕ → U
  SINGLE : Header-Name → U
  STR : ℕ → U
  VERSION : U
  METHOD CODE : U
  REQUEST-URI REASON-PHRASE : U
  HEADER-NAME : U
  HEADER-VALUE : Header-Name → U

El : U → Set
El CHAR = Char
El NAT = ℕ
El (DAR n) = Dar n
El (DAR-RANGE n m) = DarRange n m true
El (SINGLE x) = Single x
El (STR n) = Vec Char n
El VERSION = Version
El METHOD = Method
El CODE = Code
El REQUEST-URI = Request-URI
El REASON-PHRASE = Reason-Phrase
El HEADER-NAME = Header-Name
El (HEADER-VALUE h) = Header-Value h

mutual
  data Format : Set where
    Fail End : Format
    Base : U → Format
    Upto : Format → Format → Format
    Slurp : Format → Format
    Skip Or And : Format → Format → Format
    Use : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Fail ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base u ⟧ = El u
  ⟦ Upto _ f ⟧ = ⟦ f ⟧
  ⟦ Slurp f ⟧ = List ⟦ f ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ Or f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ And f₁ f₂ ⟧ = ⟦ f₁ ⟧ × ⟦ f₂ ⟧
  ⟦ Use f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

_>>_ : Format → Format → Format
f₁ >> f₂ = Skip f₁ f₂

_>>-_ : Format → Format → Format
x >>- y = And x y

_>>=_ : (f : Format) → (⟦ f ⟧ → Format) → Format
x >>= y = Use x y

_∣_ : Format → Format → Format
x ∣ y = Or x y

char : Char → Format
char c = Base (DAR (toNat c))

str : String → Format
str s = chars (toList s)
  where
  chars : List Char → Format
  chars [] = End
  chars (x ∷ xs) = char x >>- chars xs

DIGIT = Base (DAR-RANGE (toNat '0') (toNat '9'))
SP    = Base (DAR 32)
CR    = Base (DAR 13)
LF    = Base (DAR 10)
CRLF  = CR >>- LF
End-Headers  = CRLF >>- CRLF

Required-Header : Header-Name → Format
Required-Header h =
  Upto End-Headers (
    Base (SINGLE h) >>= λ h →
    char ':' >>
    SP >>
    Base (HEADER-VALUE (proj h)) >>-
    CRLF >>
    End
  )

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

Request-Format =
  Base METHOD >>= λ m →
  SP >>
  Base REQUEST-URI >>-
  SP >>
  Base VERSION >>-
  CRLF >>  
  Remaining-Format m

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

Response-Format : Method → Format
Response-Format m =
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
