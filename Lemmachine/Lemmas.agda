open import Lemmachine
module Lemmachine.Lemmas (properties : Properties) where
open import Lemmachine.Resource.Configure
open import Data.List.Any hiding (any)
open Membership-≡ public
open import Relation.Binary.PropositionalEquality public

resource : Resource
resource = toResource properties

stub : Properties → Resource
stub overrides = configure resource overrides

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

methodIsKnown : ∀ res req → Request.method req ∈ Resource.knownMethods res req
                          → any (eqMethod (Request.method req))
                                (Resource.knownMethods res req) ≡ true
methodIsKnown res req p = methodIsMember req (Resource.knownMethods res req) p

methodIsAllowed : ∀ res req → Request.method req ∈ Resource.allowedMethods res req
                            → any (eqMethod (Request.method req))
                                  (Resource.allowedMethods res req) ≡ true
methodIsAllowed res req p = methodIsMember req (Resource.allowedMethods res req) p
