module Lemmachine.Resource where
open import Lemmachine.Request
open import Lemmachine.Response
open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Bool

Hook : Set → Set
Hook A = Request → A

record Config : Set where
  field
    resourceExists : Request → Bool
    serviceAvailable : Request → Bool
    isAuthorized : Request → AuthHead
    forbidden : Request → Bool
    allowMissingPost : Request → Bool
    malformedRequest : Request → Bool
    uriTooLong : Request → Bool
    knownContentType : Request → Bool
    validContentHeaders : Request → Bool
    validEntityLength : Request → Bool
    options : Request → List ResponseHeader
    allowedMethods : Request → List Method
    knownMethods : Request → List Method
    deleteResource : Request → Bool
    deleteCompleted : Request → Bool
    postIsCreate : Request → Bool
    createPath : Request → Maybe Path
    processPost : Request → Bool
    contentTypesProvided : Request → List (MediaType × Handler)
    languageAvailable : Request → Bool
    contentTypesAccepted : Request → List (MediaType × Handler)
    charsetsProvided : Request → List (Charset × CharsetConverter)
    encodingsProvided : Request → List (Encoding × Encoder)
    variances : Request → List RequestHeader
    isConflict : Request → Bool
    multipleChoices : Request → Bool
    previouslyExisted : Request → Bool
    movedPermanently : Request → Maybe MovedURI
    movedTemporarily : Request → Maybe MovedURI
    lastModified : Request → Maybe DateTime
    expires : Request → Maybe DateTime
    generateETag : Request → Maybe ETag
    finishRequest : Request → Bool

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

data Code : Set where
  resourceExists : Code
  serviceAvailable : Code
  isAuthorized : Code
  forbidden : Code
  allowMissingPost : Code
  malformedRequest : Code
  uriTooLong : Code
  knownContentType : Code
  validContentHeaders : Code
  validEntityLength : Code
  options : Code
  allowedMethods : Code
  knownMethods : Code
  deleteResource : Code
  deleteCompleted : Code
  postIsCreate : Code
  createPath : Code
  processPost : Code
  contentTypesProvided : Code
  languageAvailable : Code
  contentTypesAccepted : Code
  charsetsProvided : Code
  encodingsProvided : Code
  variances : Code
  isConflict : Code
  multipleChoices : Code
  previouslyExisted : Code
  movedPermanently : Code
  movedTemporarily : Code
  lastModified : Code
  expires : Code
  generateETag : Code
  finishRequest : Code
