module Lemmachine.Utils where
open import Lemmachine.Request
open import Lemmachine.Response
open import Data.Maybe
open import Data.Bool hiding (_≟_)
open import Data.String
open import Data.Function
open import Data.Product
open import Relation.Nullary
open import Data.List hiding (any)
open import Data.List.Any
open Membership-≡

Application = Request → Response
Middleware = Application → Application

fetch : String → List (String × String) → Maybe String
fetch x xs with any (_≟_ x ∘ proj₁) xs
... | yes p = just (proj₂ (proj₁ (find p)))
... | no _ = nothing

fetchContentType : String → List (String × String) → Maybe String
fetchContentType _ [] = nothing
fetchContentType "*" (x ∷ _) = just (proj₂ x)
fetchContentType "*/*" (x ∷ _) = just (proj₂ x)
fetchContentType c (x ∷ xs) = fetch c (x ∷ xs)

private
  fromHeaders : RequestHeaders → List (String × String)
  fromHeaders xs = Data.List.map f xs where
    f : RequestHeader → String × String
    f (k , v) = k , v

fetchHeader : String → RequestHeaders → Maybe String
fetchHeader x xs with any (_≟_ x ∘ headerKey) xs
... | yes p = just (headerValue (proj₁ (find p)))
... | no _ = nothing

postulate
  isDate : String → Bool
  isModified : String → String → Bool
  now : String
