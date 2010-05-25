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

private
  fromHeaders : RequestHeaders → List (String × String)
  fromHeaders xs = Data.List.map f xs where
    f : RequestHeader → String × String
    f (k , v) = k , v

fetchHeader : String → RequestHeaders → Maybe String
fetchHeader x xs with any (_≟_ x ∘ headerKey) xs
... | yes p = just (headerValue (proj₁ (find p)))
... | no _ = nothing

fetchContentType : String → List String → Maybe String
fetchContentType _ [] = nothing
fetchContentType "*" (y ∷ _) = just y
fetchContentType "*/*" (y ∷ _) = just y
fetchContentType x (y ∷ ys) with x == y
... | true = just y
... | false = fetchContentType x ys

fetchAccept : RequestHeaders → List String → Maybe String
fetchAccept hs cs with fetchHeader "Accept" hs
fetchAccept hs _ | nothing = nothing
fetchAccept _ [] | just _ = nothing
fetchAccept _ xss | just y = fetchContentType y xss

postulate
  isDate : String → Bool
  isModified : String → String → Bool
  now : String
