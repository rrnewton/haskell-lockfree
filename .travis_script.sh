#!/bin/bash

set -xe

if [ "$STACKVER" == "" ]; then
# Legacy cabal version; soon to be deprecated:

 (cd ./atomic-primops/testing && cabal configure --enable-tests && cabal test --show-details=always)
 (cd ./atomic-primops-foreign && cabal configure --enable-tests && cabal test --show-details=always)
 (cd ./abstract-deque   &&       cabal configure --enable-tests && cabal test --show-details=always)
 (cd ./lockfree-queue// &&       cabal configure --enable-tests && cabal test --show-details=always)
 (cd ./chaselev-deque/  &&       cabal configure --enable-tests && cabal test --show-details=always)
 (cd ./mega-deque/      &&       cabal configure --enable-tests && cabal test --show-details=always)

else
 stack --stack-yaml=stack-${STACK_RESOLVER}.yaml test --no-terminal
fi
