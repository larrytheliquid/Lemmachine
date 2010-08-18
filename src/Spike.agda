module Spike where
open import Data.Bool
open import Data.Nat
open import Data.String hiding (_==_)

data Method : Set where
  GET HEAD POST : Method

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
