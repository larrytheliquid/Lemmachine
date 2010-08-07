module Abstract where
open import Data.Bool
open import Data.Char
open import Data.String
open import Data.Nat
open import Data.Maybe
open import Data.List

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

postulate
  Method : Set
  request-target : Set
  
  HTTP-Version : Set

  Status-Code : Set
  Reason-Phrase : Set

  field-name : Set
  field-value : field-name → Set

data Request : Set where
  Request-Line : Method → request-target → HTTP-Version → Request
  header-field : (n : field-name) → field-value n → Request → Request
  message-body : List OCTET → Request → Request

data Response : Set where
  Status-Line  : HTTP-Version → Status-Code → Reason-Phrase → Response
  header-field : (n : field-name) → field-value n → Response → Response
  message-body : List OCTET → Response → Response
