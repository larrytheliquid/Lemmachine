module Lemmachine.HTTP where
open import Lemmachine.Data
open import Data.Bool
open import Data.Nat
open import Data.String
open import Data.Maybe
open import Relation.Nullary
open import Relation.Binary
open import Relation.Binary.PropositionalEquality

data Version : Set where
  HTTP/1∶0 : Version

Request-URI = String
Reason-Phrase = String

data Header-Name : Set where
  Date Pragma : Header-Name

  Authorization From : Header-Name
  If-Modified-Since : Header-Name
  Referer User-Agent : Header-Name

  Location Server : Header-Name
  WWW-Authenticate : Header-Name

  Allow Content-Encoding : Header-Name
  Content-Length Content-Type : Header-Name
  Expires Last-Modified : Header-Name

Header-Value : Header-Name → Set
Header-Value Content-Length = ℕ
Header-Value _ = String

data Method : Set where
  GET HEAD POST : Method

data Code : Set where
  200-OK 201-Created : Code
  202-Accepted 204-No-Content : Code

  300-Multiple-Choices 301-Moved-Permanently : Code
  302-Moved-Temporarily 304-Not-Modified : Code

  400-Bad-Request 401-Unauthorized : Code
  403-Forbidden 404-Not-Found : Code

  500-Internal-Server-Error : Code
  501-Not-Implemented : Code
  502-Bad-Gateway : Code
  503-Service-Unavailable : Code

_m≟_ : (x y : Header-Name) → Maybe (Dec (x ≡ y))

Date m≟ Date = just (yes refl)
Pragma m≟ Pragma = just (yes refl)

Authorization m≟ Authorization = just (yes refl)
From m≟ From = just (yes refl)
If-Modified-Since m≟ If-Modified-Since = just (yes refl)
Referer m≟ Referer = just (yes refl)
User-Agent m≟ User-Agent = just (yes refl)

Location m≟ Location = just (yes refl)
Server m≟ Server = just (yes refl)
WWW-Authenticate m≟ WWW-Authenticate = just (yes refl)

Allow m≟ Allow = just (yes refl)
Content-Encoding m≟ Content-Encoding = just (yes refl)
Content-Length m≟ Content-Length = just (yes refl)
Content-Type m≟ Content-Type = just (yes refl)
Expires m≟ Expires = just (yes refl)
Last-Modified m≟ Last-Modified = just (yes refl)

_ m≟ _ = nothing
