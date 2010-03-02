module Lemmachine.Lemmas where
open import Lemmachine.Request
open import Lemmachine.Status
open import Lemmachine.Utils
open import Data.Bool
open import Data.List
open import Data.Maybe
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Data.Function using (const)
open import Lemmachine.Resource
open import Lemmachine.DecisionCore

serviceUnavailable : ∀ {r} → resolve (configure [ serviceAvailable , const false ]) r ≡ ServiceUnavailable
serviceUnavailable = refl

unknownMethod : ∀ {r} → resolve (configure [ knownMethods , const [] ]) r ≡ NotImplemented
unknownMethod = refl

requestURItooLong : ∀ {r} → resolve (configure [ uriTooLong , const true ]) r ≡ RequestURItooLong
requestURItooLong {r} with Request.method r
... | HEAD = refl
... | GET = refl
... | PUT = refl
... | DELETE = refl
... | POST = refl
... | TRACE = refl
... | CONNECT = refl
... | OPTIONS = refl

disallowedMethod : ∀ {r} → resolve (configure [ allowedMethods , const [] ]) r ≡ MethodNotAllowed
disallowedMethod {r} with Request.method r
... | HEAD = refl
... | GET = refl
... | PUT = refl
... | DELETE = refl
... | POST = refl
... | TRACE = refl
... | CONNECT = refl
... | OPTIONS = refl

badRequest : ∀ {r} → B9 (configure [ malformedRequest , const true ]) r ≡ BadRequest
badRequest = refl

unauthorized : ∀ {r} → B8 (configure [ isAuthorized , const false ]) r ≡ Unauthorized
unauthorized = refl

lem-forbidden : ∀ {r} → B7 (configure [ forbidden , const true ]) r ≡ Forbidden
lem-forbidden = refl

invalidContentHeaders : ∀ {r} → B6 (configure [ validContentHeaders , const false ]) r ≡ NotImplemented
invalidContentHeaders = refl

unsupportedMediaType : ∀ {r} → B5 (configure [ knownContentType , const false ]) r ≡ UnsupportedMediaType
unsupportedMediaType = refl

invalidEntityLength : ∀ {r} → B4 (configure [ validEntityLength , const false ]) r ≡ RequestEntityTooLarge
invalidEntityLength = refl

optionsSuccess : ∀ {r}(_ : Request.method r ≡ OPTIONS) → B3 (configure []) r ≡ OK
optionsSuccess p rewrite p = refl

preconditionFailed : ∀ {r}(_ : fetch "If-Match" (Request.headers r) ≡ just "*") 
                     → H7 (configure []) r ≡ PreconditionFailed
preconditionFailed {r} p with fetch "If-Match" (Request.headers r) | p
... | ._ | refl = refl
