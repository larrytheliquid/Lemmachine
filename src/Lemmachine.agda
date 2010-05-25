module Lemmachine where
open import Lemmachine.Request public
open import Lemmachine.Response public
open import Lemmachine.Resource public
open import Lemmachine.Resource.Configure public
open import Lemmachine.Resolve
open import Lemmachine.Utils public
open import Lemmachine.Runner public

open import Data.Bool public hiding (_≟_)
open import Data.List public

toApp : Hooks → Application
toApp hs = resolve (toResource hs)
