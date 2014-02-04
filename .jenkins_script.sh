#!/bin/bash

set -e
set -x

source $HOME/rn_jenkins_scripts/acquire_ghc.sh
which cabal
cabal --version

which -a llc || echo "No LLVM"

# AtomicPrimops/testing/
PKGS="atomic-primops/ atomic-primops/testing/ atomic-primops-foreign/ abstract-deque/ lockfree-queue/ chaselev-deque/ mega-deque/"
MODE1="--enable-library-profiling --enable-executable-profiling"
MODE2="--disable-library-profiling --disable-executable-profiling"

cabal sandbox init

root=`pwd`
for subdir in $PKGS; do 
  cd "$root/$subdir"
  cabal sandbox init --sandbox=$root
done
cd "$root"


# First install everything without testing:
CMDROOT="cabal install --reinstall --with-ghc=ghc-$JENKINS_GHC --force-reinstalls"
$CMDROOT $MODE2 $PKGS

# Now install the DEPENDENCIES for testing
$CMDROOT $MODE2 $PKGS --enable-tests --only-dependencies

# List what we've got:
cabal sandbox hc-pkg list

echo "Everything installed, now to test."
for subdir in $PKGS; do 
  cd "$root/$subdir"
  cabal sandbox hc-pkg list
  # Print the individual test outputs:
  cabal test --show-details=always
done
