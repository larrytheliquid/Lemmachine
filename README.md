Lemmachine
==========

## Setup ##

    cabal install hack hack-handler-happstack

## Development ##

We use a vendored modified Agda stdlib, so in Emacs:
    M-x customize group
    agda2

Then add this to `Agda2 Include Dirs`:
    ./vendor/stdlib/src

## Running ##

Lemmachine currently supports resolving HTTP status codes (resolving
headers & body will come later).

Run the following to see this in action for `Lemmachine.Resource.Default`:
    agda -c -i . -i ./vendor/stdlib/src Lemmachine/Runner.agda
    ./Runner
    curl -i http://localhost:3000 && echo
