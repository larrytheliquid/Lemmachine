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

B7 : Request → Status
B7 r with forbidden r
... | true  = Forbidden
... | false = END r

B8 : Request → Status
B8 r with isAuthorized r
... | true  = B7 r
... | false = Unauthorized

B9 : Request → Status
B9 r with malformedRequest r
... | true  = BadRequest
... | false = B8 r

B10 : Request → Status
B10 r with any (eqMethod (Request.method r))
               (allowedMethods r)
... | true  = B9 r
... | false = MethodNotAllowed

B11 : Request → Status
B11 r with uriTooLong r
... | true  = RequestURItooLong
... | false = B10 r

B12 : Request → Status
B12 r with any (eqMethod (Request.method r))
               (knownMethods r)
... | true  = B11 r 
... | false = NotImplemented

B13 : Request → Status
B13 r with serviceAvailable r 
... | true  = B12 r 
... | false = ServiceUnavailable

decide : Request → Status
decide r = B13 r
