module Lemmachine.Request where
open import Data.Bool

AuthHead = Bool

data Method : Set where
  HEAD GET PUT DELETE POST : Method
  TRACE CONNECT OPTIONS : Method

data Request : Set where
