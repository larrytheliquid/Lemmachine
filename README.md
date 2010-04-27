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

Lemmachine statuses can presently be passed through to Hack.

To see this in action, run the following from the project root directory:
    agda -c -i . -i ./vendor/stdlib/src Lemmachine/Runner.agda
    ./Runner
    curl -i http://localhost:3000 && echo
