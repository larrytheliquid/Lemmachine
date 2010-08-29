module HTTP where
open import Data
open import Data.Bool
open import Data.Maybe
open import Relation.Nullary
open import Relation.Binary
open import Relation.Binary.PropositionalEquality

data Header : Set where
  Date Pragma : Header

  Authorization From : Header
  If-Modified-Since : Header
  Referer User-Agent : Header

  Location Server : Header
  WWW-Authenticate : Header

  Allow Content-Encoding : Header
  Content-Length Content-Type : Header
  Expires Last-Modified : Header

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

data Role : Set where
  Request-Sender : Role
  Request-Recipient : Role
  Response-Sender : Role
  Response-Recipient : Role

Params : Role → Set
Params Request-Sender = Method
Params Request-Recipient = Method
Params Response-Sender = Method × Code
Params Response-Recipient = Method × Code

data El : (r : Role) → Params r → (required : Bool) → Set where
  Date1 : El Request-Sender POST false
  Date2 : ∀ {m c} → El Response-Sender (m , c) true

_m≟_ : (x y : Header) → Maybe (Dec (x ≡ y))

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
