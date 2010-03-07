module Lemmachine.Utils where
open import Lemmachine.Request
open import Lemmachine.Response.Status
open import Data.Maybe
open import Data.Bool hiding (_≟_)
open import Data.String
open import Data.Function
open import Data.Product
open import Relation.Nullary
open import Data.List hiding (any)
open import Data.List.Any
open Membership-≡

Application = Request → Status
Middleware = Application → Application

fetch : String → List (String × String) → Maybe String
fetch x xs with any (_≟_ x ∘ proj₁) xs
... | yes p = just (proj₂ (proj₁ (find p)))
... | no _ = nothing

postulate isDate : String → Bool
postulate isModified : String → String → Bool
postulate now : String
