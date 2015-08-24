#!/bin/bash

set -xe

(cd ./atomic-primops/testing && cabal configure --enable-tests && cabal test --show-details=always)
(cd ./atomic-primops-foreign && cabal configure --enable-tests && cabal test --show-details=always)
(cd ./abstract-deque   &&       cabal configure --enable-tests && cabal test --show-details=always)
(cd ./lockfree-queue// &&       cabal configure --enable-tests && cabal test --show-details=always)
(cd ./chaselev-deque/  &&       cabal configure --enable-tests && cabal test --show-details=always)
(cd ./mega-deque/      &&       cabal configure --enable-tests && cabal test --show-details=always)
