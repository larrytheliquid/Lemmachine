module Lemmachine.Runner where
open import Lemmachine
import Lemmachine.Resource.Default as Default
open import Data.Unit
open import Data.Nat
open import Data.String
open import Foreign.Haskell
open import IO.Primitive

{-# IMPORT FFI #-}
postulate run : Status → IO Unit
{-# COMPILED run FFI.run #-}

main = run OK -- (resolve (toResource Default.resource))

