module Lemmachine.Resource where
open import Lemmachine.Request
open import Data.Bool
open import Data.List

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
