open import Lemmachine
module Lemmachine.Lemmas (hooks : Hooks) where
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
private
  resource : Resource
  resource = toResource hooks
open Request public
open Resource resource public

resolveStatus : Request → Status
resolveStatus r = resolve resource r

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
                   → method r ∈ methods
                   → any (eqMethod (method r))
                         methods ≡ true
  methodIsMember _ [] ()
  methodIsMember _ (x ∷ _) (here p) rewrite p with eqMethod-refl x
  ... | p₂ rewrite p₂ = refl
  methodIsMember r (x ∷ xs) (there ps) with eqMethod (method r) x | methodIsMember r xs ps
  ... | true | _ = refl
  ... | false | p rewrite p = refl

methodIsKnown : ∀ r → method r ∈ knownMethods r
                    → any (eqMethod (method r))
                          (knownMethods r) ≡ true
methodIsKnown r p = methodIsMember r (knownMethods r) p

methodIsAllowed : ∀ r → method r ∈ allowedMethods r
                      → any (eqMethod (method r))
                            (allowedMethods r) ≡ true
methodIsAllowed r p = methodIsMember r (allowedMethods r) p

postulate
  methodIsntAllowed : ∀ r → method r ∉ allowedMethods r
                          → any (eqMethod (method r))
                                (allowedMethods r) ≡ false

notOptions : ∀ r → method r ≢ OPTIONS
                 → eqMethod (method r) OPTIONS ≡ false
notOptions r p with method r
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
  ... | no _ | v₂ , p with any₂ (_≟_ header ∘ headerKey) xs
  ... | yes _ rewrite p = v₂ , refl
  ... | no _ =  v₂ , p

acceptIsHeader : ∀ r → "Accept" ∈ map headerKey (headers r)
                     → ∃ λ v → fetchHeader "Accept" (headers r) ≡ just v
acceptIsHeader r p with headerIsMember "Accept" (headers r) p
... | v , p₂ with fetchHeader "Accept" (headers r) | p₂
... | ._ | refl = v , refl

acceptLanguageIsHeader : ∀ r → "Accept-Language" ∈ map headerKey (headers r)
                             → ∃ λ v → fetchHeader "Accept-Language" (headers r) ≡ just v
acceptLanguageIsHeader r p with headerIsMember "Accept-Language" (headers r) p
... | v , p₂ with fetchHeader "Accept-Language" (headers r) | p₂
... | ._ | refl = v , refl
