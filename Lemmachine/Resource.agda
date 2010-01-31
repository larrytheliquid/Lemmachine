module Lemmachine.Resource where
open import Lemmachine.Request
open import Data.Bool

data Resource : Set where
  resource :
    (forbidden : Request) -> Bool
    -> Resource

forbidden : Request -> Bool
forbidden _ = false


