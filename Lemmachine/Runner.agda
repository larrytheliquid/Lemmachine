module Lemmachine.Runner where
{-# IMPORT FFI #-}
open import Lemmachine
import Lemmachine.Resource.Default as Default
open import Data.Unit
open import Data.Nat
open import Data.String
open import Foreign.Haskell
open import IO.Primitive

postulate run : (Data-Request → Status) → IO Unit
{-# COMPILED run FFI.run #-}

main = run (Data-resolve (toResource Default.resource))

