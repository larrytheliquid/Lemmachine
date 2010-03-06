module Lemmachine.Lemmas where
open import Lemmachine.Request
open import Lemmachine.Resource
open import Data.Bool
open import Data.List
open import Data.List.Any hiding (any)
open Membership-≡
open import Relation.Binary.PropositionalEquality

private
  eqMethod-refl : ∀ m → eqMethod m m ≡ true
  eqMethod-refl HEAD = refl
  eqMethod-refl GET = refl
  eqMethod-refl PUT = refl
  eqMethod-refl DELETE = refl
  eqMethod-refl POST = refl
  eqMethod-refl TRACE = refl
  eqMethod-refl CONNECT = refl
  eqMethod-refl OPTIONS = refl

  methodIsMember : ∀ r → (methods : List Method)
                   → Request.method r ∈ methods
                   → any (eqMethod (Request.method r))
                         methods ≡ true
  methodIsMember _ [] ()
  methodIsMember _ (x ∷ _) (here p) rewrite p with eqMethod-refl x
  ... | p₂ rewrite p₂ = refl
  methodIsMember r (x ∷ xs) (there ps) with eqMethod (Request.method r) x | methodIsMember r xs ps
  ... | true | _ = refl
  ... | false | p rewrite p = refl

methodIsKnown : ∀ c r → Request.method r ∈ Config.knownMethods c r
                      → any (eqMethod (Request.method r))
                            (Config.knownMethods c r) ≡ true
methodIsKnown c r p = methodIsMember r (Config.knownMethods c r) p

methodIsAllowed : ∀ c r → Request.method r ∈ Config.allowedMethods c r
                        → any (eqMethod (Request.method r))
                              (Config.allowedMethods c r) ≡ true
methodIsAllowed c r p = methodIsMember r (Config.allowedMethods c r) p
