module Lemmachine.DecisionCore where
open import Lemmachine.Resource
open import Lemmachine.Request
open import Lemmachine.Status
open import Data.Bool
open import Data.Nat
open import Relation.Binary
open import Relation.Binary.PropositionalEquality
open import Data.List

END : Request → Status
END _ = OK

B12 : Request → Status
B12 r = if any 
           (eqMethod (Request.method r))
           (knownMethods r)
  then END r 
  else NotImplemented

B13 : Request → Status
B13 r = if serviceAvailable r then B12 r else ServiceUnavailable

decide : Request → Status
decide r = B13 r
