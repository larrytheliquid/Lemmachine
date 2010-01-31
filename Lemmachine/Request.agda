module Lemmachine.Request where

data Request : Set where

data Method : Set where
  HEAD GET PUT DELETE POST : Method
  TRACE CONNECT OPTIONS : Method
