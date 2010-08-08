module Abstract where
open import Data.Bool
open import Data.Char
open import Data.String
open import Data.Nat
open import Data.Maybe
open import Data.Vec

between? : Char → ℕ → ℕ → Bool
between? c start end = toBool lower ∧ toBool higher
  where
  target = toNat c
  lower  = suc target ∸ start
  higher = suc end ∸ target
  toBool : ℕ → Bool
  toBool zero    = false
  toBool (suc _) = true

data Between? (start end : ℕ) : Bool → Set where
  char : (c : Char) → Between? start end (between? c 0 end)

OCTET = Between? 0 255 true

data Method : Set where
  HEAD : Method

request-target = String
HTTP-Version = String

data field-name : Set where
  Host : field-name

field-value : field-name → Set
field-value Host = String

postulate
  Status-Code : Set
  Reason-Phrase : Set

data Request' : Bool → Set where
  Request-Line : Method → request-target → HTTP-Version → Request' false
  header-field : (name : field-name) → field-value name → Request' false → Request' false
  message-body : ∀ {n} → Vec OCTET n → Request' false → Request' true

Request : {_ : Bool} → Set
Request {b} = Request' b

data Response' : Bool → Set where
  Status-Line  : HTTP-Version → Status-Code → Reason-Phrase → Response' false
  header-field : (n : field-name) → field-value n → Response' false → Response' false
  message-body : ∀ {n} → Vec OCTET n → Response' false → Response' true

Response : {_ : Bool} → Set
Response {b} = Response' b
