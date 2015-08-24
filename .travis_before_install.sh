#!/bin/bash

set -xe

sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install cabal-install-$CABALVER ghc-$GHCVER # see note about happy/alex
