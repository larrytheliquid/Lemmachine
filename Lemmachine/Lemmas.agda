module Lemmachine.Lemmas where
open import Lemmachine.Request
open import Lemmachine.Status
open import Lemmachine.Utils
open import Data.Bool
open import Data.List
open import Data.List.Any hiding (any)
open Membership-≡
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

private
  methodIsKnown : ∀ r → any (eqMethod (Request.method r))
                        (Config.knownMethods (configure []) r) 
                    ≡ true
  methodIsKnown r with Request.method r
  ... | HEAD = refl
  ... | GET = refl
  ... | PUT = refl
  ... | DELETE = refl
  ... | POST = refl
  ... | TRACE = refl
  ... | CONNECT = refl
  ... | OPTIONS = refl
  
  allowedRequest : Request → Set
  allowedRequest r = Request.method r ∈ Config.allowedMethods (configure []) r

  methodIsAllowed : ∀ r → allowedRequest r
                      → any (eqMethod (Request.method r))
                          (Config.allowedMethods (configure []) r) 
                      ≡ true
  methodIsAllowed r (here p) rewrite p = refl
  methodIsAllowed r (there (here p)) rewrite p = refl
  methodIsAllowed r (there (there ()))

requestURItooLong : ∀ {r} → resolve (configure [ uriTooLong , const true ]) r ≡ RequestURItooLong
requestURItooLong {r} with methodIsKnown r
... | p rewrite p = refl

disallowedMethod : ∀ {r} → resolve (configure [ allowedMethods , const [] ]) r ≡ MethodNotAllowed
disallowedMethod {r} with methodIsKnown r
... | p rewrite p = refl

badRequest : ∀ {r} → allowedRequest r → resolve (configure [ malformedRequest , const true ]) r ≡ BadRequest
badRequest {r} m with methodIsKnown r | methodIsAllowed r m
... | p | p₂ rewrite p | p₂ = refl

unauthorized : ∀ {r} → allowedRequest r → resolve (configure [ isAuthorized , const false ]) r ≡ Unauthorized
unauthorized {r} m with methodIsKnown r | methodIsAllowed r m
... | p | p₂ rewrite p | p₂ = refl

lem-forbidden : ∀ {r} → allowedRequest r → resolve (configure [ forbidden , const true ]) r ≡ Forbidden
lem-forbidden {r} m with methodIsKnown r | methodIsAllowed r m
... | p | p₂ rewrite p | p₂ = refl

invalidContentHeaders : ∀ {r} → allowedRequest r → resolve (configure [ validContentHeaders , const false ]) r ≡ NotImplemented
invalidContentHeaders {r} m with methodIsKnown r | methodIsAllowed r m
... | p | p₂ rewrite p | p₂ = refl

unsupportedMediaType : ∀ {r} → allowedRequest r → resolve (configure [ knownContentType , const false ]) r ≡ UnsupportedMediaType
unsupportedMediaType {r} m with methodIsKnown r | methodIsAllowed r m
... | p | p₂ rewrite p | p₂ = refl

invalidEntityLength : ∀ {r} → allowedRequest r → resolve (configure [ validEntityLength , const false ]) r ≡ RequestEntityTooLarge
invalidEntityLength {r} m with methodIsKnown r | methodIsAllowed r m
... | p | p₂ rewrite p | p₂ = refl

optionsSuccess : ∀ {r} → Request.method r ≡ OPTIONS → B3 (configure []) r ≡ OK
optionsSuccess p rewrite p = refl

preconditionFailed : ∀ {r} → fetch "If-Match" (Request.headers r) ≡ just "*"
                     → H7 (configure []) r ≡ PreconditionFailed
preconditionFailed {r} p with fetch "If-Match" (Request.headers r) | p
... | ._ | refl = refl
