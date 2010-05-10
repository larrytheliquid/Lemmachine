module Lemmachine.Proofs where
open import Lemmachine
import Lemmachine.Resource.Default
import Lemmachine.Lemmas
open Lemmachine.Lemmas Lemmachine.Resource.Default.resource
open import Relation.Binary.PropositionalEquality
open import Data.Empty
open import Data.Maybe
open import Data.Product hiding (map)
open import Data.Function using (const)

serviceUnavailable : ∀ r → resolveStatus (stub [ serviceAvailable ⇒ const false ]) r ≡ ServiceUnavailable
serviceUnavailable _ = refl

notImplemented : ∀ r → resolveStatus (stub [ knownMethods ⇒ const [] ]) r ≡ NotImplemented
notImplemented _ = refl

tooLongResource = stub [ uriTooLong ⇒ const true ]
requestURItooLong : ∀ r → Request.method r ∈ Resource.knownMethods tooLongResource r
                        → resolveStatus tooLongResource r ≡ RequestURItooLong
requestURItooLong r p with methodIsKnown tooLongResource r p
... | p₂ rewrite p₂ = refl

notAllowedResource = stub [ allowedMethods ⇒ const [] ]
methodNotAllowed : ∀ r → Request.method r ∈ Resource.knownMethods notAllowedResource r
                       → resolveStatus notAllowedResource r ≡ MethodNotAllowed
methodNotAllowed r p with methodIsKnown notAllowedResource r p
... | p₂ rewrite p₂ = refl

malformedResource = stub [ malformedRequest ⇒ const true ]
badRequest : ∀ r → Request.method r ∈ Resource.knownMethods malformedResource r
                 → Request.method r ∈ Resource.allowedMethods malformedResource r
                 → resolveStatus malformedResource r ≡ BadRequest
badRequest r p p₂ with methodIsKnown malformedResource r p | methodIsAllowed malformedResource r p₂
... | p₃ | p₄ rewrite p₃ | p₄ = refl

unauthorizedResource = stub [ isAuthorized ⇒ const false ]
unauthorized : ∀ r → Request.method r ∈ Resource.knownMethods unauthorizedResource r
                   → Request.method r ∈ Resource.allowedMethods unauthorizedResource r
                   → resolveStatus unauthorizedResource r ≡ Unauthorized
unauthorized r p p₂ with methodIsKnown unauthorizedResource r p | methodIsAllowed unauthorizedResource r p₂
... | p₃ | p₄ rewrite p₃ | p₄ = refl

forbiddenResource = stub [ forbidden ⇒ const true ]
lem-forbidden : ∀ r → Request.method r ∈ Resource.knownMethods forbiddenResource r
                    → Request.method r ∈ Resource.allowedMethods forbiddenResource r
                    → resolveStatus forbiddenResource r ≡ Forbidden
lem-forbidden r p p₂ with methodIsKnown forbiddenResource r p | methodIsAllowed forbiddenResource r p₂
... | p₃ | p₄ rewrite p₃ | p₄ = refl

invalidContentHeadersResource = stub [ validContentHeaders ⇒ const false ]
invalidContentHeaders : ∀ r → Request.method r ∈ Resource.knownMethods invalidContentHeadersResource r
                            → Request.method r ∈ Resource.allowedMethods invalidContentHeadersResource r
                            → resolveStatus invalidContentHeadersResource r ≡ NotImplemented
invalidContentHeaders r p p₂ with methodIsKnown invalidContentHeadersResource r p | methodIsAllowed invalidContentHeadersResource r p₂
... | p₃ | p₄ rewrite p₃ | p₄ = refl

unknownContentTypeResource = stub [ knownContentType ⇒ const false ]
unsupportedMediaType : ∀ r → Request.method r ∈ Resource.knownMethods unknownContentTypeResource r
                           → Request.method r ∈ Resource.allowedMethods unknownContentTypeResource r
                           → resolveStatus unknownContentTypeResource r ≡ UnsupportedMediaType
unsupportedMediaType r p p₂ with methodIsKnown unknownContentTypeResource r p | methodIsAllowed unknownContentTypeResource r p₂
... | p₃ | p₄ rewrite p₃ | p₄ = refl

invalidEntityLengthResource = stub [ validEntityLength ⇒ const false ]
invalidEntityLength : ∀ r → Request.method r ∈ Resource.knownMethods invalidEntityLengthResource r
                          → Request.method r ∈ Resource.allowedMethods invalidEntityLengthResource r
                          → resolveStatus invalidEntityLengthResource r ≡ RequestEntityTooLarge
invalidEntityLength r p p₂ with methodIsKnown invalidEntityLengthResource r p | methodIsAllowed invalidEntityLengthResource r p₂
... | p₃ | p₄ rewrite p₃ | p₄ = refl

optionsOK : ∀ r → Request.method r ∈ Resource.knownMethods resource r
                → Request.method r ∈ Resource.allowedMethods resource r
                → Request.method r ≡ OPTIONS
                → resolveStatus resource r ≡ OK
optionsOK r p p₂ p₃ with methodIsKnown resource r p | methodIsAllowed resource r p₂
... | p₄ | p₅ rewrite p₄ | p₅ | p₃ = refl

notAcceptableResource = stub [ contentTypesProvided ⇒ const [] ]
notAcceptable : ∀ r → Request.method r ∈ Resource.knownMethods notAcceptableResource r
                    → Request.method r ∈ Resource.allowedMethods notAcceptableResource r
                    → Request.method r ≢ OPTIONS
                    → "Accept" ∈ map headerKey (Request.headers r)
                    → resolveStatus notAcceptableResource r ≡ NotAcceptable
notAcceptable r p p₂ p₃ p₄ with methodIsKnown notAcceptableResource r p | methodIsAllowed notAcceptableResource r p₂
                              | acceptIsHeader r p₄ | notOptions r p₃
notAcceptable r _ _ p₃ _ | p₅ | p₆ | v , p₇ | p₈ rewrite p₅ | p₆ | p₈ with fetchHeader "Accept" (Request.headers r) | p₇
... | ._ | refl = refl

preconditionFailed : ∀ r → fetchHeader "If-Match" (Request.headers r) ≡ just "*"
                         → H7 (stub []) r ≡ PreconditionFailed
preconditionFailed r p with fetchHeader "If-Match" (Request.headers r) | p
... | ._ | refl = refl
