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

private
  anyMethod : ∀ r → any (eqMethod (Request.method r))
                        (Config.knownMethods (configure []) r) 
                    ≡ true
  anyMethod r with Request.method r
  ... | HEAD = refl
  ... | GET = refl
  ... | PUT = refl
  ... | DELETE = refl
  ... | POST = refl
  ... | TRACE = refl
  ... | CONNECT = refl
  ... | OPTIONS = refl
  
  getRequest : Request → Set
  getRequest r = Request.method r ≡ GET

  getIsKnown : ∀ r → getRequest r
                    → any (eqMethod (Request.method r))
                          (Config.allowedMethods (configure []) r) 
                    ≡ true
  getIsKnown r p rewrite p = refl

requestURItooLong : ∀ {r} → resolve (configure [ uriTooLong , const true ]) r ≡ RequestURItooLong
requestURItooLong {r} with anyMethod r
... | p rewrite p = refl

disallowedMethod : ∀ {r} → resolve (configure [ allowedMethods , const [] ]) r ≡ MethodNotAllowed
disallowedMethod {r} with anyMethod r
... | p rewrite p = refl

badRequest : ∀ {r} → getRequest r → resolve (configure [ malformedRequest , const true ]) r ≡ BadRequest
badRequest {r} get with anyMethod r | getIsKnown r get
... | p | p₂ rewrite p | p₂ = refl

unauthorized : ∀ {r} → getRequest r → resolve (configure [ isAuthorized , const false ]) r ≡ Unauthorized
unauthorized {r} get with anyMethod r | getIsKnown r get
... | p | p₂ rewrite p | p₂ = refl

lem-forbidden : ∀ {r} → getRequest r → resolve (configure [ forbidden , const true ]) r ≡ Forbidden
lem-forbidden {r} get with anyMethod r | getIsKnown r get
... | p | p₂ rewrite p | p₂ = refl

invalidContentHeaders : ∀ {r} → getRequest r → resolve (configure [ validContentHeaders , const false ]) r ≡ NotImplemented
invalidContentHeaders {r} get with anyMethod r | getIsKnown r get
... | p | p₂ rewrite p | p₂ = refl

unsupportedMediaType : ∀ {r} → getRequest r → resolve (configure [ knownContentType , const false ]) r ≡ UnsupportedMediaType
unsupportedMediaType {r} get with anyMethod r | getIsKnown r get
... | p | p₂ rewrite p | p₂ = refl

invalidEntityLength : ∀ {r} → getRequest r → resolve (configure [ validEntityLength , const false ]) r ≡ RequestEntityTooLarge
invalidEntityLength {r} get with anyMethod r | getIsKnown r get
... | p | p₂ rewrite p | p₂ = refl

optionsSuccess : ∀ {r} → Request.method r ≡ OPTIONS → B3 (configure []) r ≡ OK
optionsSuccess p rewrite p = refl

preconditionFailed : ∀ {r} → fetch "If-Match" (Request.headers r) ≡ just "*"
                     → H7 (configure []) r ≡ PreconditionFailed
preconditionFailed {r} p with fetch "If-Match" (Request.headers r) | p
... | ._ | refl = refl
