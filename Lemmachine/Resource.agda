module Lemmachine.Resource where
open import Lemmachine.Request
open import Data.Bool

data Resource : Set where

forbidden : Request -> Bool
forbidden _ = false
