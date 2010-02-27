module Lemmachine.Lemmas where
open import Lemmachine.DecisionCore
open import Lemmachine.Request
open import Lemmachine.Status
open import Data.Bool
open import Relation.Binary.PropositionalEquality

serviceUnavailable : ∀ {r} → B13 (λ _ → false) r ≡ ServiceUnavailable
serviceUnavailable = refl

requestURItooLong : ∀ {r} → B11 (λ _ → true) r ≡ RequestURItooLong
requestURItooLong = refl

badRequest : ∀ {r} → B9 (λ _ → true) r ≡ BadRequest
badRequest = refl

unauthorized : ∀ {r} → B8 (λ _ → false) r ≡ Unauthorized
unauthorized = refl

forbidden : ∀ {r} → B7 (λ _ → true) r ≡ Forbidden
forbidden = refl

invalidContentHeaders : ∀ {r} → B6 (λ _ → false) r ≡ NotImplemented
invalidContentHeaders = refl

unsupportedMediaType : ∀ {r} → B5 (λ _ → false) r ≡ UnsupportedMediaType
unsupportedMediaType = refl

invalidEntityLength : ∀ {r} → B4 (λ _ → false) r ≡ RequestEntityTooLarge
invalidEntityLength = refl
