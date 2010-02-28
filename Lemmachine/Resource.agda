module Lemmachine.Resource where
open import Lemmachine.Request
open import Lemmachine.Response
open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Bool

module Universe where
  data Code : Set where
    resourceExists serviceAvailable isAuthorized : Code
    forbidden allowMissingPost malformedRequest : Code
    uriTooLong knownContentType validContentHeaders : Code
    validEntityLength options allowedMethods : Code
    knownMethods deleteResource deleteCompleted : Code
    postIsCreate createPath processPost : Code
    contentTypesProvided languageAvailable contentTypesAccepted : Code
    charsetsProvided encodingsProvided variances : Code
    isConflict multipleChoices previouslyExisted : Code
    movedPermanently movedTemporarily lastModified : Code
    expires generateETag finishRequest : Code

  convert : Code → Set
  convert resourceExists = Request → Bool
  convert serviceAvailable = Request → Bool
  convert isAuthorized = Request → AuthHead
  convert forbidden = Request → Bool
  convert allowMissingPost = Request → Bool
  convert malformedRequest = Request → Bool
  convert uriTooLong = Request → Bool
  convert knownContentType = Request → Bool
  convert validContentHeaders = Request → Bool
  convert validEntityLength = Request → Bool
  convert options = Request → List ResponseHeader
  convert allowedMethods = Request → List Method
  convert knownMethods = Request → List Method
  convert deleteResource = Request → Bool
  convert deleteCompleted = Request → Bool
  convert postIsCreate = Request → Bool
  convert createPath = Request → Maybe Path
  convert processPost = Request → Bool
  convert contentTypesProvided = Request → List (MediaType × Handler)
  convert languageAvailable = Request → Bool
  convert contentTypesAccepted = Request → List (MediaType × Handler)
  convert charsetsProvided = Request → List (Charset × CharsetConverter)
  convert encodingsProvided = Request → List (Encoding × Encoder)
  convert variances = Request → List RequestHeader
  convert isConflict = Request → Bool
  convert multipleChoices = Request → Bool
  convert previouslyExisted = Request → Bool
  convert movedPermanently = Request → Maybe MovedURI
  convert movedTemporarily = Request → Maybe MovedURI
  convert lastModified = Request → Maybe DateTime
  convert expires = Request → Maybe DateTime
  convert generateETag = Request → Maybe ETag
  convert finishRequest = Request → Bool

private
  module U = Universe

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

id : U.Code → Config → Config
id fn c = record {
    resourceExists = Config.resourceExists c
  ; serviceAvailable = Config.serviceAvailable c
  ; isAuthorized = Config.isAuthorized c
  ; forbidden = Config.forbidden c
  ; allowMissingPost = Config.allowMissingPost c
  ; malformedRequest = Config.malformedRequest c
  ; uriTooLong = Config.uriTooLong c
  ; knownContentType = Config.knownContentType c
  ; validContentHeaders = Config.validContentHeaders c
  ; validEntityLength = Config.validEntityLength c
  ; options = Config.options c
  ; allowedMethods = Config.allowedMethods c
  ; knownMethods = Config.knownMethods c
  ; deleteResource = Config.deleteResource c
  ; deleteCompleted = Config.deleteCompleted c
  ; postIsCreate = Config.postIsCreate c
  ; createPath = Config.createPath c
  ; processPost = Config.processPost c
  ; contentTypesProvided = Config.contentTypesProvided c
  ; languageAvailable = Config.languageAvailable c
  ; contentTypesAccepted = Config.contentTypesAccepted c
  ; charsetsProvided = Config.charsetsProvided c
  ; encodingsProvided = Config.encodingsProvided c
  ; variances = Config.variances c
  ; isConflict = Config.isConflict c
  ; multipleChoices = Config.multipleChoices c
  ; previouslyExisted = Config.previouslyExisted c
  ; movedPermanently = Config.movedPermanently c
  ; movedTemporarily = Config.movedTemporarily c
  ; lastModified = Config.lastModified c
  ; expires = Config.expires c
  ; generateETag = Config.generateETag c
  ; finishRequest = Config.finishRequest c
  }
