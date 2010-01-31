module Lemmachine.Utils where

data Default (A : Set) : Set where
  default : Default A
  value : A -> Default A
