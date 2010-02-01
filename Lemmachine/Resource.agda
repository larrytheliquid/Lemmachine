module Lemmachine.Resource where
open import Lemmachine.Request
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

uriTooLong : Request → Bool
uriTooLong _ = false

knownContentType : Request → Bool
knownContentType _ = true

validContentHeaders : Request → Bool
validContentHeaders _ = true

validEntityLength : Request → Bool
validEntityLength _ = true

options : Request → List Header
options _ = []

allowedMethods : Request → List Method
allowedMethods _ = HEAD ∷ GET ∷ []

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

contentTypesProvided : Request → MediaType × Handler
contentTypesProvided _ = "text/html" , toHtml
