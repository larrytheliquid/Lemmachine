open import Lemmachine
module Lemmachine.Lemmas (properties : Properties) where
open import Data.Empty
open import Data.String
open import Data.Maybe
open import Data.Function
open import Data.Product hiding (map)
open import Data.List.Any hiding (map) renaming (any to any₂)
open import Relation.Nullary
open Membership-≡ public
open import Relation.Binary.PropositionalEquality public
open import Relation.Binary.PropositionalEquality.TrustMe

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

notOptions : ∀ r → Request.method r ≢ OPTIONS
                 → eqMethod (Request.method r) OPTIONS ≡ false
notOptions r p with Request.method r
... | HEAD = refl
... | GET = refl
... | PUT = refl
... | DELETE = refl
... | POST = refl
... | TRACE = refl
... | CONNECT = refl
... | OPTIONS = ⊥-elim (p refl)

private
  ==-refl : ∀ s → (s == s) ≡ true
  ==-refl s = trustMe

  headerIsMember : (header : String)
                   → (headers : RequestHeaders)
                   → header ∈ map headerKey headers
                   → ∃ λ v → fetchHeader header headers ≡ just v
  headerIsMember _ [] ()
  headerIsMember _ ((k , v) ∷ _) (here p) rewrite p with ==-refl k
  ... | p₂ rewrite p₂ = v , refl
  headerIsMember header ((k , v) ∷ xs) (there ps) with header ≟ k | headerIsMember header xs ps
  ... | yes _ | _ = v , refl
  ... | no _ | (v₂ , p) with any₂ (_≟_ header ∘ headerKey) xs
  ... | yes _ = {!!} -- rewrite p = v₂ , refl
  ... | no _ = {!!} -- v₂ , p

-- acceptIsHeader : ∀ req → "Accept" ∈ map headerKey (Request.headers req)
--                        → ∃ λ v → fetch "Accept" (Request.headers req) ≡ just v
-- acceptIsHeader req p with headerIsMember "Accept" (Request.headers req) p
-- ... | v , p₂ with fetch "Accept" (Request.headers req) | p₂
-- ... | ._ | refl = v , refl
