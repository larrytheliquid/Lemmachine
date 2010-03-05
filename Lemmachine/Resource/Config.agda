module Lemmachine.Resource.Config where
import Lemmachine.Resource.Universe
open import Lemmachine.Request
open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Bool
open import Data.Function using (const)

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
    resourceExists = const true
  ; serviceAvailable = const true
  ; isAuthorized = const true
  ; forbidden = const false
  ; allowMissingPost = const false
  ; malformedRequest = const false
  ; uriTooLong = const false
  ; knownContentType = const true
  ; validContentHeaders = const true
  ; validEntityLength = const true
  ; options = const []
  ; allowedMethods = const (HEAD ∷ GET ∷ [])
  ; knownMethods = const (HEAD ∷ GET ∷ PUT ∷ DELETE ∷ POST ∷ TRACE ∷ CONNECT ∷ OPTIONS ∷ [])
  ; deleteResource = const false
  ; deleteCompleted = const true
  ; postIsCreate = const false
  ; createPath = const nothing
  ; processPost = const false
  ; contentTypesProvided = const [ "text/html" , "toHtml" ]
  ; languageAvailable = const true
  ; contentTypesAccepted = const []
  ; charsetsProvided = const []
  ; encodingsProvided = const [ "identity" , "defaultEncoder" ]
  ; variances = const []
  ; isConflict = const false
  ; multipleChoices = const false
  ; previouslyExisted = const false
  ; movedPermanently = const nothing
  ; movedTemporarily = const nothing
  ; lastModified = const nothing
  ; expires = const nothing
  ; generateETag = const nothing
  ; finishRequest = const true
  }
