module Lemmachine.Resource.Universe where
open import Lemmachine.Request
open import Lemmachine.Response
open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Bool
open import Data.String

AuthHead = Bool
MediaType = String
Handler = String
Charset = String
CharsetConverter = String
Encoding = String
Encoder = String
MovedURI = String
DateTime = String
ETag = String

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
