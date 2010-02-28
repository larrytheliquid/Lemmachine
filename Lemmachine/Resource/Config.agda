module Lemmachine.Resource.Config where
import Lemmachine.Resource.Universe
open import Lemmachine.Request
open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Bool

private
  module U = Lemmachine.Resource.Universe

record Config : Set where
  field
    resourceExists :  U.convert U.resourceExists
    serviceAvailable : U.convert U.serviceAvailable
    isAuthorized : U.convert U.isAuthorized
    forbidden : U.convert U.forbidden
    allowMissingPost : U.convert U.allowMissingPost
    malformedRequest : U.convert U.malformedRequest
    uriTooLong : U.convert U.uriTooLong
    knownContentType : U.convert U.knownContentType
    validContentHeaders : U.convert U.validContentHeaders
    validEntityLength : U.convert U.validEntityLength
    options : U.convert U.options
    allowedMethods : U.convert U.allowedMethods
    knownMethods : U.convert U.knownMethods
    deleteResource : U.convert U.deleteResource
    deleteCompleted : U.convert U.deleteCompleted
    postIsCreate : U.convert U.postIsCreate
    createPath : U.convert U.createPath
    processPost : U.convert U.processPost
    contentTypesProvided : U.convert U.contentTypesProvided
    languageAvailable : U.convert U.languageAvailable
    contentTypesAccepted : U.convert U.contentTypesAccepted
    charsetsProvided : U.convert U.charsetsProvided
    encodingsProvided : U.convert U.encodingsProvided
    variances : U.convert U.variances
    isConflict : U.convert U.isConflict
    multipleChoices : U.convert U.multipleChoices
    previouslyExisted : U.convert U.previouslyExisted
    movedPermanently : U.convert U.movedPermanently
    movedTemporarily : U.convert U.movedTemporarily
    lastModified : U.convert U.lastModified
    expires : U.convert U.expires
    generateETag : U.convert U.generateETag
    finishRequest : U.convert U.finishRequest

default : Config
default = record {
    resourceExists = λ _ → true
  ; serviceAvailable = λ _ → true
  ; isAuthorized = λ _ → true
  ; forbidden = λ _ → false
  ; allowMissingPost = λ _ → false
  ; malformedRequest = λ _ → false
  ; uriTooLong = λ _ → false
  ; knownContentType = λ _ → true
  ; validContentHeaders = λ _ → true
  ; validEntityLength = λ _ → true
  ; options = λ _ → []
  ; allowedMethods = λ _ → HEAD ∷ GET ∷ []
  ; knownMethods = λ _ → HEAD ∷ GET ∷ PUT ∷ DELETE ∷ POST ∷ TRACE ∷ CONNECT ∷ OPTIONS ∷ []
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
