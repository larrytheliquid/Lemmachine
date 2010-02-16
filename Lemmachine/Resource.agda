module Lemmachine.Resource where
open import Lemmachine.Request
open import Lemmachine.Response
open import Data.Maybe
open import Data.List
open import Data.Product
open import Data.Bool

resourceExists : Request → Bool
resourceExists _ = true

serviceAvailable : Request → Bool
serviceAvailable _ = true

isAuthorized : Request → AuthHead
isAuthorized _ = true

forbidden : Request → Bool
forbidden _ = false

allowMissingPost : Request → Bool
allowMissingPost _ = false

malformedRequest : Request → Bool
malformedRequest _ = false

uriTooLong : Request → Bool
uriTooLong _ = false

knownContentType : Request → Bool
knownContentType _ = true

validContentHeaders : Request → Bool
validContentHeaders _ = true

validEntityLength : Request → Bool
validEntityLength _ = true

options : Request → List ResponseHeader
options _ = []

allowedMethods : Request → List Method
allowedMethods _ = HEAD ∷ GET ∷ []

knownMethods : Request → List Method
knownMethods _ = HEAD ∷ GET ∷ PUT ∷ DELETE ∷ POST ∷ TRACE ∷ CONNECT ∷ OPTIONS ∷ []

deleteResource : Request → Bool
deleteResource _ = false

deleteCompleted : Request → Bool
deleteCompleted _ = true

postIsCreate : Request → Bool
postIsCreate _ = false

createPath : Request → Maybe Path
createPath _ = nothing

processPost : Request → Bool
processPost _ = false

contentTypesProvided : Request → List (MediaType × Handler)
contentTypesProvided _ = [ "text/html" , "toHtml" ]

contentTypesAccepted : Request → List (MediaType × Handler)
contentTypesAccepted _ = []

charsetsProvided : Request → Maybe (Charset × CharsetConverter)
charsetsProvided _ = nothing

encodingsProvided : Request → List (Encoding × Encoder)
encodingsProvided _ = [ "identity" , defaultEncoder ]

variances : Request → List VaryHeader
variances _ = []

isConflict : Request → Bool
isConflict _ = false

multipleChoices : Request → Bool
multipleChoices _ = false

previouslyExisted : Request → Bool
previouslyExisted _ = false

movedPermanently : Request → Maybe MovedURI
movedPermanently _ = nothing

movedTemporarily : Request → Maybe MovedURI
movedTemporarily _ = nothing

lastModified : Request → Maybe DateTime
lastModified _ = nothing

expires : Request → Maybe DateTime
expires _ = nothing

generateETag : Request → Maybe ETag
generateETag _ = nothing

finishRequest : Request → Bool
finishRequest _ = true
