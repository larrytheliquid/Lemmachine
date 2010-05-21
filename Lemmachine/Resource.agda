module Lemmachine.Resource where
import Lemmachine.Resource.Universe
open import Data.Product
open import Data.List

private
  module U = Lemmachine.Resource.Universe
  open U using (convert; Code)

Hook = Σ Code convert
Hooks = List Hook

_⇒_ : {A : Set}{B : A → Set} → (a : A) → B a → (Σ A B)
_⇒_ = _,_

record Resource : Set where
  field
    resourceExists : convert U.resourceExists
    serviceAvailable : convert U.serviceAvailable
    isAuthorized : convert U.isAuthorized
    forbidden : convert U.forbidden
    allowMissingPost : convert U.allowMissingPost
    malformedRequest : convert U.malformedRequest
    uriTooLong : convert U.uriTooLong
    knownContentType : convert U.knownContentType
    validContentHeaders : convert U.validContentHeaders
    validEntityLength : convert U.validEntityLength
    options : convert U.options
    allowedMethods : convert U.allowedMethods
    knownMethods : convert U.knownMethods
    deleteResource : convert U.deleteResource
    deleteCompleted : convert U.deleteCompleted
    postIsCreate : convert U.postIsCreate
    createPath : convert U.createPath
    processPost : convert U.processPost
    contentTypesProvided : convert U.contentTypesProvided
    languageAvailable : convert U.languageAvailable
    contentTypesAccepted : convert U.contentTypesAccepted
    charsetsProvided : convert U.charsetsProvided
    encodingsProvided : convert U.encodingsProvided
    variances : convert U.variances
    isConflict : convert U.isConflict
    multipleChoices : convert U.multipleChoices
    previouslyExisted : convert U.previouslyExisted
    movedPermanently : convert U.movedPermanently
    movedTemporarily : convert U.movedTemporarily
    lastModified : convert U.lastModified
    expires : convert U.expires
    generateETag : convert U.generateETag
    finishRequest : convert U.finishRequest
