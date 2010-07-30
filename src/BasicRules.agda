module BasicRules where
open import Data.Empty
open import Data.Bool
open import Data.Char
open import Data.String hiding (_++_)
open import Data.Nat hiding (_*_)
open import Data.Sum
open import Data.Product
open import Data.List hiding ([_];_++_)

infixr 1 _∣_ _++_
infix 3 _*_[_] _*[_] *_[_]

∞ : ℕ
∞ = 0

data BList (A : Set) : ℕ → Set where
  [] : ∀ {n} → BList A n
  _∷_ : ∀ {n} → A → BList A n → BList A (suc n)

data Cons (A B : Set) : Set where
  _∷_ : A → B → Cons A B

FList : Set → ℕ → ℕ → Set
FList A zero (suc m)    = BList A (suc m)
FList A zero ∞          = List A
FList A (suc n) zero    = Cons A (FList A n zero)
FList A (suc n) (suc m) = Cons A (FList A n m)

_*_[_] : ℕ → ℕ → Set → Set
n * m [ A ] = FList A n m

_*[_] : ℕ → Set → Set
n *[ A ] = FList A n ∞

*_[_] : ℕ → Set → Set
* n [ A ] = FList A zero n

[_] : Set → Set
[ A ] =  * 1 [ A ]

_∣_ : (A B : Set) → Set
A ∣ B = A ⊎ B

_++_ : (A B : Set) → Set
A ++ B = A × B

data U : Set where
  DAR NAT : U

El : U → Set
El DAR = Char
El NAT = ℕ

data Dar : ℕ → Set where
  char : (c : Char) → Dar (toNat c)

between? : Char → ℕ → ℕ → Bool
between? c start end = toBool lower ∧ toBool higher
  where
  target = toNat c
  lower  = suc target ∸ start
  higher = suc end ∸ target
  toBool : ℕ → Bool
  toBool zero    = false
  toBool (suc _) = true

data AnyDar (start end : ℕ) : Bool → Set where
  char : (c : Char) → AnyDar start end (between? c start end)

US-ASCII : {u : U} → String → El u → Set
US-ASCII {DAR} _ x = Dar (toNat x)
US-ASCII {NAT} _ x = Dar x

any-US-ASCII : {u : U} → String → El u → El u → Set
any-US-ASCII {DAR} _ x y = AnyDar (toNat x) (toNat y) true
any-US-ASCII {NAT} _ x y = AnyDar x y true

-- http://tools.ietf.org/html/rfc2616#section-2.2

OCTET   = any-US-ASCII "8-bit sequence of data" 0 255
CHAR    = any-US-ASCII "character" 0 127
UPALPHA = any-US-ASCII "uppercase letter" 'A' 'Z'
LOALPHA = any-US-ASCII "lowercase letter" 'a' 'z'
ALPHA   = UPALPHA ∣ LOALPHA
DIGIT   = any-US-ASCII "digit" '0' '9'
CTL     = any-US-ASCII "control character" 0 31 ∣ US-ASCII "DEL" 127
CR      = US-ASCII "carriage return" 13
LF      = US-ASCII "linefeed" 10
SP      = US-ASCII "space" 32
HT      = US-ASCII "horizontal-tab" 9
DQ      = US-ASCII "double-quote mark" 34

CRLF = CR ++ LF

LWS = [ CR ] ++ 1 *[ SP ∣ HT ]

HEX = any-US-ASCII "hexadecimal uppercase letters" 'A' 'F'
    ∣ any-US-ASCII "hexadecimal lowercase letters" 'a' 'f'
    ∣ DIGIT

separators
  = US-ASCII "" '(' ∣ US-ASCII "" ')' ∣ US-ASCII "" '<' ∣ US-ASCII "" '>' ∣ US-ASCII "" '@'
   ∣ US-ASCII "" ',' ∣ US-ASCII "" ';' ∣ US-ASCII "" ':' ∣ US-ASCII "" '\\' ∣ US-ASCII "" '"'
   ∣ US-ASCII "" '/' ∣ US-ASCII "" '[' ∣ US-ASCII "" ']' ∣ US-ASCII "" '?' ∣ US-ASCII "" '='
   ∣ US-ASCII "" '{' ∣ US-ASCII "" '}' ∣ SP ∣ HT
