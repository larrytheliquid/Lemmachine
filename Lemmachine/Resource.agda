module Lemmachine.Resource where
open import Lemmachine.Request
open import Data.Bool

record Resource : Set where
  field 
    forbidden : Request -> Bool

forbidden : Request -> Bool
forbidden _ = false


