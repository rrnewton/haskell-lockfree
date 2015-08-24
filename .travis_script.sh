#!/bin/bash

set -xe

(cd ./atomic-primops/testing && cabal test --show-details=always)
(cd ./atomic-primops-foreign && cabal configure && cabal test --show-details=always)
(cd ./abstract-deque   &&       cabal configure && cabal test --show-details=always)
(cd ./lockfree-queue// &&       cabal configure && cabal test --show-details=always)
(cd ./chaselev-deque/  &&       cabal configure && cabal test --show-details=always)
(cd ./mega-deque/      &&       cabal configure && cabal test --show-details=always)
