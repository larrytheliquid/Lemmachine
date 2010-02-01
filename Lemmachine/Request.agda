module Lemmachine.Request where
open import Data.Bool
open import Data.String

AuthHead = Bool
Header = String

data Method : Set where
  HEAD GET PUT DELETE POST : Method
  TRACE CONNECT OPTIONS : Method

data Request : Set where
