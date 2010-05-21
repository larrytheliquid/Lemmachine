module Lemmachine.Resource where
import Lemmachine.Resource.Universe
open import Data.Product
open import Data.List

private
  module U = Lemmachine.Resource.Universe
  open U using (El; Code)

Hook = Σ Code El
Hooks = List Hook

_⇒_ : {A : Set}{B : A → Set} → (a : A) → B a → (Σ A B)
_⇒_ = _,_

record Resource : Set where
  field
    resourceExists : El U.resourceExists
    serviceAvailable : El U.serviceAvailable
    isAuthorized : El U.isAuthorized
    forbidden : El U.forbidden
    allowMissingPost : El U.allowMissingPost
    malformedRequest : El U.malformedRequest
    uriTooLong : El U.uriTooLong
    knownContentType : El U.knownContentType
    validContentHeaders : El U.validContentHeaders
    validEntityLength : El U.validEntityLength
    options : El U.options
    allowedMethods : El U.allowedMethods
    knownMethods : El U.knownMethods
    deleteResource : El U.deleteResource
    deleteCompleted : El U.deleteCompleted
    postIsCreate : El U.postIsCreate
    createPath : El U.createPath
    processPost : El U.processPost
    contentTypesProvided : El U.contentTypesProvided
    languageAvailable : El U.languageAvailable
    contentTypesAccepted : El U.contentTypesAccepted
    charsetsProvided : El U.charsetsProvided
    encodingsProvided : El U.encodingsProvided
    variances : El U.variances
    isConflict : El U.isConflict
    multipleChoices : El U.multipleChoices
    previouslyExisted : El U.previouslyExisted
    movedPermanently : El U.movedPermanently
    movedTemporarily : El U.movedTemporarily
    lastModified : El U.lastModified
    expires : El U.expires
    generateETag : El U.generateETag
    finishRequest : El U.finishRequest
