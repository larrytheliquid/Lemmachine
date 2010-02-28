module Lemmachine.Lemmas where
open import Lemmachine.Request
open import Lemmachine.Status
open import Lemmachine.Utils
open import Data.Bool
open import Data.List
open import Data.Maybe
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Lemmachine.Resource
import Lemmachine.DecisionCore

fail = record {
    resourceExists = λ _ → true
  ; serviceAvailable = λ _ → false
  ; isAuthorized = λ _ → false
  ; forbidden = λ _ → true
  ; allowMissingPost = λ _ → false
  ; malformedRequest = λ _ → true
  ; uriTooLong = λ _ → true
  ; knownContentType = λ _ → false
  ; validContentHeaders = λ _ → false
  ; validEntityLength = λ _ → false
  ; options = λ _ → []
  ; allowedMethods = λ _ → []
  ; knownMethods = λ _ → []
  ; deleteResource = λ _ → false
  ; deleteCompleted = λ _ → true
  ; postIsCreate = λ _ → false
  ; createPath = λ _ → nothing
  ; processPost = λ _ → false
  ; contentTypesProvided = λ _ → [ "text/html" , "toHtml" ]
  ; languageAvailable = λ _ → true
  ; contentTypesAccepted = λ _ → []
  ; charsetsProvided = λ _ → []
  ; encodingsProvided = λ _ → [ "identity" , "defaultEncoder" ]
  ; variances = λ _ → []
  ; isConflict = λ _ → false
  ; multipleChoices = λ _ → false
  ; previouslyExisted = λ _ → false
  ; movedPermanently = λ _ → nothing
  ; movedTemporarily = λ _ → nothing
  ; lastModified = λ _ → nothing
  ; expires = λ _ → nothing
  ; generateETag = λ _ → nothing
  ; finishRequest = λ _ → true
  }

open Lemmachine.DecisionCore fail

serviceUnavailable : ∀ {r} → B13 r ≡ ServiceUnavailable
serviceUnavailable = refl

unknownMethod : ∀ {r} → B12 r ≡ NotImplemented
unknownMethod = refl

requestURItooLong : ∀ {r} → B11 r ≡ RequestURItooLong
requestURItooLong = refl

disallowedMethod : ∀ {r} → B10 r ≡ MethodNotAllowed
disallowedMethod = refl

badRequest : ∀ {r} → B9 r ≡ BadRequest
badRequest = refl

unauthorized : ∀ {r} → B8 r ≡ Unauthorized
unauthorized = refl

forbidden : ∀ {r} → B7 r ≡ Forbidden
forbidden = refl

invalidContentHeaders : ∀ {r} → B6 r ≡ NotImplemented
invalidContentHeaders = refl

unsupportedMediaType : ∀ {r} → B5 r ≡ UnsupportedMediaType
unsupportedMediaType = refl

invalidEntityLength : ∀ {r} → B4 r ≡ RequestEntityTooLarge
invalidEntityLength = refl

optionsSuccess : ∀ {r}(_ : Request.method r ≡ OPTIONS) → B3 r ≡ OK
optionsSuccess p rewrite p = refl

preconditionFailed : ∀ {r}(_ : fetch "If-Match" (Request.headers r) ≡ just "*") → H7 r ≡ PreconditionFailed
preconditionFailed {r} p with fetch "If-Match" (Request.headers r) | p
... | ._ | refl = refl
