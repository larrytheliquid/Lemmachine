module Lemmachine.Default where
open import Lemmachine

resource : Hooks
resource = []

main = runResolve (toApp resource)
