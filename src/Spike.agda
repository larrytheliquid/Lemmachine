module Spike where
open import Data.Bool
open import Data.Nat
open import Data.String hiding (_==_)

data Method : Set where
  GET HEAD POST : Method

Request-URI = String
Reason-Phrase = String

data GET-Header : Set where
  Pragma : GET-Header
  Authorization From : GET-Header
  If-Modified-Since : GET-Header
  Referer User-Agent : GET-Header

data HEAD-Header : Set where
  Pragma : HEAD-Header
  Authorization From : HEAD-Header
  Referer User-Agent : HEAD-Header

data POST-Header : Set where
  Date Pragma : POST-Header
  Authorization From : POST-Header
  Referer User-Agent : POST-Header
  Content-Encoding : POST-Header

  Content-Length : POST-Header
  Content-Type : POST-Header

Header : Method → Set
Header GET  = GET-Header
Header HEAD = HEAD-Header
Header POST = POST-Header

Value : {m : Method} → Header m → Set
Value {GET}  _ = String
Value {HEAD} _ = String
Value {POST} Content-Length = ℕ
Value {POST} _ = String

data GET-Response-Header : Set where
  Date Pragma : GET-Response-Header
  Location Server : GET-Response-Header
  WWW-Authenticate : GET-Response-Header
  
  Content-Length : GET-Response-Header
  Content-Type : GET-Response-Header

data HEAD-Response-Header : Set where
  Date Pragma : HEAD-Response-Header
  Location Server : HEAD-Response-Header
  WWW-Authenticate : HEAD-Response-Header

data POST-Response-Header : Set where
  Date Pragma : POST-Response-Header
  Location Server : POST-Response-Header
  WWW-Authenticate : POST-Response-Header

  Content-Length : POST-Response-Header
  Content-Type : POST-Response-Header

Response-Header : Method → Set
Response-Header GET  = GET-Response-Header
Response-Header HEAD = HEAD-Response-Header
Response-Header POST = POST-Response-Header

Response-Value : {m : Method} → Response-Header m → Set
Response-Value {GET} Content-Length = ℕ
Response-Value {GET}  _ = String
Response-Value {HEAD} _ = String
Response-Value {POST} Content-Length = ℕ
Response-Value {POST} _ = String
