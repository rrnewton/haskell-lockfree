#!/bin/bash

set -e
set -x

source $HOME/rn_jenkins_scripts/acquire_ghc.sh
which cabal
cabal --version

which -a llc || echo "No LLVM"

# AtomicPrimops/testing/
PKGS="atomic-primops/ atomic-primops/testing/ atomic-primops-foreign/ AbstractDeque/ MichaelScott/ ChaseLev/ MegaDeque/"
MODE1="--enable-library-profiling --enable-executable-profiling"
MODE2="--disable-library-profiling --disable-executable-profiling"

cabal sandbox init
CMDROOT="cabal install --reinstall --with-ghc=ghc-$JENKINS_GHC --force-reinstalls --enable-tests"
$CMDROOT $MODE2 $PKGS
