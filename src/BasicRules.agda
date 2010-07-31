module BasicRules where
open import Data.Empty
open import Data.Bool
open import Data.Char hiding (_==_)
open import Data.String hiding (_++_;_==_)
open import Data.Nat hiding (_*_)
open import Data.Sum
open import Data.Product
open import Data.List hiding ([_];_++_)

infixr 1 _∣_ _++_
infix 3 _*_[_] _*[_] *_[_]

∞ : ℕ
∞ = 0

_==_ : ℕ → ℕ → Bool
zero == zero = true
suc _ == zero = false
zero == suc _ = false
suc x == suc y = x == y

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

data RelDar (f : Char → Bool) : Bool → Set where
  char : (c : Char) → RelDar f (f c)

is? : ℕ → Char → Bool
is? n c = n == toNat c

Dar : ℕ → Set
Dar n = RelDar (is? n) true

between? : ℕ → ℕ → Char → Bool
between? start end c = toBool lower ∧ toBool higher
  where
  target = toNat c
  lower  = suc target ∸ start
  higher = suc end ∸ target
  toBool : ℕ → Bool
  toBool zero    = false
  toBool (suc _) = true

BetweenDar : ℕ → ℕ → Set
BetweenDar x y = RelDar (between? x y) true

any? : List ℕ → Char → Bool
any? xs c = any (_==_ (toNat c)) xs

AnyDar : List ℕ → Set
AnyDar xs = RelDar (any? xs) true

US-ASCII : {u : U} → String → El u → Set
US-ASCII {DAR} _ x = Dar (toNat x)
US-ASCII {NAT} _ x = Dar x

between-US-ASCII : {u : U} → String → El u → El u → Set
between-US-ASCII {DAR} _ x y = BetweenDar (toNat x) (toNat y)
between-US-ASCII {NAT} _ x y = BetweenDar x y

-- http://tools.ietf.org/html/rfc2616#section-2.2

OCTET   = between-US-ASCII "8-bit sequence of data" 0 255
CHAR    = between-US-ASCII "character" 0 127
UPALPHA = between-US-ASCII "uppercase letter" 'A' 'Z'
LOALPHA = between-US-ASCII "lowercase letter" 'a' 'z'
ALPHA   = UPALPHA ∣ LOALPHA
DIGIT   = between-US-ASCII "digit" '0' '9'
CTL     = between-US-ASCII "control character" 0 31 ∣ US-ASCII "DEL" 127
CR      = US-ASCII "carriage return" 13
LF      = US-ASCII "linefeed" 10
SP      = US-ASCII "space" 32
HT      = US-ASCII "horizontal-tab" 9
DQ      = US-ASCII "double-quote mark" 34

CRLF = CR ++ LF

LWS = [ CR ] ++ 1 *[ SP ∣ HT ]

HEX = between-US-ASCII "hexadecimal uppercase letter" 'A' 'F'
    ∣ between-US-ASCII "hexadecimal lowercase letter" 'a' 'f'
    ∣ DIGIT

separators = AnyDar (Data.List.map toNat (toList "()<>@,;:\\\"/[]?={} \t"))
