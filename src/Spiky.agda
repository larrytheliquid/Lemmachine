module Spiky where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.Char
open import Data.String
open import Data.Nat
open import Data.List hiding ([_])
open import Data.Vec hiding (_>>=_; toList; [_])
open import Data.Sum
open import Data.Product hiding (_×_)
open import Data hiding ([_])
open import Spike

infixr 3 _∣_
infixr 1 _>>_ _>>-_ _>>=_

mutual 
  data U : Set where
    CHAR NAT : U
    DAR : ℕ → U
    DAR-RANGE : ℕ → ℕ → U
    SINGLE : (u : U) → El u → U
    STR : ℕ → U
    METHOD REQUEST-URI REASON-PHRASE : U
    HEADER : Method → U
    VALUE : {m : Method} → Header m → U
    RESPONSE-HEADER : Method → U
    RESPONSE-VALUE : {m : Method} → Response-Header m → U

  El : U → Set
  El CHAR = Char
  El NAT = ℕ
  El (DAR n) = Dar n
  El (DAR-RANGE n m) = DarRange n m true
  El (SINGLE _ x) = Single x
  El (STR n) = Vec Char n
  El METHOD = Method
  El REQUEST-URI = Request-URI
  El REASON-PHRASE = Reason-Phrase
  El (HEADER m) = Header m
  El (VALUE h) = Value h
  El (RESPONSE-HEADER m) = Response-Header m
  El (RESPONSE-VALUE h) = Response-Value h

GET-HEADER  = HEADER GET
HEAD-HEADER = HEADER HEAD
POST-HEADER = HEADER POST

GET-RESPONSE-HEADER  = RESPONSE-HEADER GET
HEAD-RESPONSE-HEADER = RESPONSE-HEADER HEAD
POST-RESPONSE-HEADER = RESPONSE-HEADER POST

mutual
  data Format : Set where
    Fail End : Format
    Base : U → Format
    Upto : Format → Format → Format
    Slurp : Format → Format
    Skip Or And : Format → Format → Format
    Use : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Fail ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ Base u ⟧ = El u
  ⟦ Upto _ f ⟧ = ⟦ f ⟧
  ⟦ Slurp f ⟧ = List ⟦ f ⟧
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ Or f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ And f₁ f₂ ⟧ = ⟦ f₁ ⟧ × ⟦ f₂ ⟧
  ⟦ Use f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

_>>_ : Format → Format → Format
f₁ >> f₂ = Skip f₁ f₂

_>>-_ : Format → Format → Format
x >>- y = And x y

_>>=_ : (f : Format) → (⟦ f ⟧ → Format) → Format
x >>= y = Use x y

_∣_ : Format → Format → Format
x ∣ y = Or x y

char : Char → Format
char c = Base (DAR (toNat c))

str : String → Format
str s = chars (toList s)
  where
  chars : List Char → Format
  chars [] = End
  chars (x ∷ xs) = char x >>- chars xs

DIGIT = Base (DAR-RANGE (toNat '0') (toNat '9'))
SP    = Base (DAR 32)
CR    = Base (DAR 13)
LF    = Base (DAR 10)
CRLF  = CR >>- LF
End-Headers  = CRLF >>- CRLF

HTTP-Version-Format =
  str "HTTP" >>
  char '/' >> 
  Base NAT >>= λ major →
  char '.' >>
  Base NAT >>= λ minor →
  f major minor

  where

  f : ℕ → ℕ → Format
  f 0 9 = End
  f 1 0 = End
  f _ _ = Fail

Required-Header : (u : U) → El u → Format
Required-Header (HEADER m) v =
  Upto End-Headers (
    Base (SINGLE (HEADER m) v) >>= λ h →
    char ':' >>
    SP >>
    Base (VALUE (proj h)) >>-
    CRLF >>
    End
  )
Required-Header (RESPONSE-HEADER m) v =
  Upto End-Headers (
    Base (SINGLE (RESPONSE-HEADER m) v) >>= λ h →
    char ':' >>
    SP >>
    Base (RESPONSE-VALUE (proj h)) >>-
    CRLF >>
    End
  )
Required-Header _ _ = Fail

GET-Format : Format
GET-Format =
  Slurp (
    Base GET-HEADER >>= λ h →
    char ':' >>
    SP >>
    Base (VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

HEAD-Format : Format
HEAD-Format =
  Slurp (
    Base HEAD-HEADER >>= λ h →
    char ':' >>
    SP >>
    Base (VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

POST-Format : Format
POST-Format =
  Required-Header POST-HEADER Content-Length >>= λ c-l →
  f (Data.Product.proj₁ c-l) (Data.proj₁ (Data.Product.proj₂ c-l))

  where

  f : (s : Single {Header POST} Content-Length) → Value (proj s) → Format
  f (single ._) n =
    Required-Header POST-HEADER Content-Type >>-

    Slurp (
      Base POST-HEADER >>= λ h →
      char ':' >>
      SP >>
      Base (VALUE h) >>-
      CRLF >>
      End
    ) >>-

    CRLF >>
    Base (STR n)

Method-Format : Method → Format
Method-Format GET  = GET-Format
Method-Format HEAD = HEAD-Format
Method-Format POST = POST-Format

Request-Format =
  Base METHOD >>= λ m →
  SP >>
  Base REQUEST-URI >>-
  SP >>
  HTTP-Version-Format >>-
  CRLF >>  
  Method-Format m

Status-Code-Format =
  DIGIT >>-
  DIGIT >>-
  DIGIT

GET-Response-Format : Format
GET-Response-Format =
  Required-Header GET-RESPONSE-HEADER Date >>-

  Slurp (
    Base GET-RESPONSE-HEADER >>= λ h →
    char ':' >>
    SP >>
    Base (RESPONSE-VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

HEAD-Response-Format : Format
HEAD-Response-Format =
  Required-Header HEAD-RESPONSE-HEADER Date >>-

  Slurp (
    Base HEAD-RESPONSE-HEADER >>= λ h →
    char ':' >>
    SP >>
    Base (RESPONSE-VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

POST-Response-Format : Format
POST-Response-Format =
  Required-Header POST-RESPONSE-HEADER Date >>-

  Slurp (
    Base POST-RESPONSE-HEADER >>= λ h →
    char ':' >>
    SP >>
    Base (RESPONSE-VALUE h) >>-
    CRLF >>
    End
  ) >>-

  CRLF >>
  End

Method-Response-Format : Method → Format
Method-Response-Format GET  = GET-Response-Format
Method-Response-Format HEAD = HEAD-Response-Format
Method-Response-Format POST = POST-Response-Format

-- TODO: Properly comply with 3xx & 201 wrt optional/required
Location-Format : Method → ℕ → ℕ → ℕ → Format
Location-Format GET  3 _ _ = Required-Header GET-RESPONSE-HEADER  Location
Location-Format HEAD 3 _ _ = Required-Header HEAD-RESPONSE-HEADER Location
Location-Format POST 3 _ _ = Required-Header POST-RESPONSE-HEADER Location
Location-Format POST 2 0 1 = Required-Header POST-RESPONSE-HEADER Location
Location-Format _    _ _ _ = End

WWW-Authenticate-Format : Method → ℕ → ℕ → ℕ → Format
WWW-Authenticate-Format GET  4 0 1 = Required-Header GET-RESPONSE-HEADER  WWW-Authenticate
WWW-Authenticate-Format HEAD 4 0 1 = Required-Header HEAD-RESPONSE-HEADER WWW-Authenticate
WWW-Authenticate-Format POST 4 0 1 = Required-Header POST-RESPONSE-HEADER WWW-Authenticate
WWW-Authenticate-Format _    _ _ _ = End

Response-Format : Method → Format
Response-Format m =
  HTTP-Version-Format >>-
  SP >>
  Status-Code-Format >>= λ s-c →
  ( λ (n₁ n₂ n₃ : ℕ) →

    guard m n₁ n₂ n₃ >>
    SP >>
    Base REASON-PHRASE >>-
    CRLF >>
    Location-Format m n₁ n₂ n₃ >>-
    WWW-Authenticate-Format m n₁ n₂ n₃ >>-
    Method-Response-Format m

  ) (nat (Data.proj₁ s-c))
    (nat (Data.proj₁ (Data.proj₂ s-c)))
    (nat (Data.proj₂ (Data.proj₂ s-c)))

  where

  guard : Method → ℕ → ℕ → ℕ → Format
  guard _    1 _ _ = Fail
  guard GET  2 0 1 = Fail
  guard HEAD 2 0 1 = Fail
  guard _    _ _ _ = End
