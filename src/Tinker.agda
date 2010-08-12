module Tinker where
open import Data.String
open import Data.Nat
open import Data.Product
open import Data.Sum
open import Data.Vec

data METHOD : Set where
  OPTIONS HEAD : METHOD

data Req : Set where
  Method Accept Content-Length : Req

Req-El : Req → Set
Req-El Content-Length = ℕ ⊎ ((n : ℕ) → Vec String n)
Req-El _ = String

-- Request = Σ Req Req-El

data Resp : Set where
  Transfer-Encoding : Resp

  Content-Type : Resp
  Content-Length : Resp

  message-body : Resp

-- Resp-El : Req-El Accept → Resp → Set
-- Resp-El _ Content-Type = String

-- Reponse = Σ Resp Resp-El

