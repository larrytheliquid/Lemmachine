module Lemmachine.Resource where
open import Lemmachine.Request
open import Data.Bool

data Resource : Set where
  resource :
    (resourceExists : Request -> Bool)
    (serviceAvailable : Request -> Bool)
    (isAuthorized : Request -> Bool)
    (forbidden : Request -> Bool)
    -> Resource

resourceExists : Request -> Bool
resourceExists _ = true

serviceAvailable : Request -> Bool
serviceAvailable _ = true

isAuthorized : Request -> Bool
isAuthorized _ = true

forbidden : Request -> Bool
forbidden _ = false


