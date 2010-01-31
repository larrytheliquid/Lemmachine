module Lemmachine.Request where

data Request : Set where

data Method : Set where
  GET HEAD POST PUT DELETE : Method
  TRACE CONNECT OPTIONS : Method
