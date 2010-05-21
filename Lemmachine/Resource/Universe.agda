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

El : Code → Set
El resourceExists = Request → Bool
El serviceAvailable = Request → Bool
El isAuthorized = Request → AuthHead
El forbidden = Request → Bool
El allowMissingPost = Request → Bool
El malformedRequest = Request → Bool
El uriTooLong = Request → Bool
El knownContentType = Request → Bool
El validContentHeaders = Request → Bool
El validEntityLength = Request → Bool
El options = Request → List ResponseHeader
El allowedMethods = Request → List Method
El knownMethods = Request → List Method
El deleteResource = Request → Bool
El deleteCompleted = Request → Bool
El postIsCreate = Request → Bool
El createPath = Request → Maybe Path
El processPost = Request → Bool
El contentTypesProvided = Request → List (MediaType × Handler)
El languageAvailable = Request → Bool
El contentTypesAccepted = Request → List (MediaType × Handler)
El charsetsProvided = Request → List (Charset × CharsetConverter)
El encodingsProvided = Request → List (Encoding × Encoder)
El variances = Request → List RequestHeader
El isConflict = Request → Bool
El multipleChoices = Request → Bool
El previouslyExisted = Request → Bool
El movedPermanently = Request → Maybe MovedURI
El movedTemporarily = Request → Maybe MovedURI
El lastModified = Request → Maybe DateTime
El expires = Request → Maybe DateTime
El generateETag = Request → Maybe ETag
El finishRequest = Request → Bool
