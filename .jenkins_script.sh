#!/bin/bash

set -e
set -x

source $HOME/rn_jenkins_scripts/acquire_ghc.sh
which cabal
cabal --version

which -a llc || echo "No LLVM"

# AtomicPrimops/testing/
PKGS="atomic-primops/ atomic-primops/testing/ atomic-primops-foreign/ abstract-deque/ abstract-deque-tests/ lockfree-queue/ chaselev-deque/ mega-deque/"

# Pass OPTLVL directly to cabal:
CBLARGS=" $OPTLVL "

if [ "$PROF" == "prof" ]; then 
  CBLARGS="$CBLARGS --enable-library-profiling --enable-executable-profiling"
else
  CBLARGS="$CBLARGS --disable-library-profiling --disable-executable-profiling"
fi

if [ "$HPC" == "hpc" ]; then 
  CBLARGS="$CBLARGS --enable-library-coverage"
else
  CBLARGS="$CBLARGS --disable-library-coverage"
fi

if [ "$THREADING" == "nothreads" ]; then 
  echo "Compiling without threading support."
else
  CBLARGS="$CBLARGS --ghc-options=-threaded "
fi

cabal sandbox init

root=`pwd`
for subdir in $PKGS; do 
  cd "$root/$subdir"
  cabal sandbox init --sandbox=$root/.cabal-sandbox
done
cd "$root"


# First install everything without testing:
CMDROOT="cabal install --reinstall --with-ghc=ghc-$JENKINS_GHC --force-reinstalls"
$CMDROOT $CBLARGS $PKGS

# Now install the DEPENDENCIES for testing
$CMDROOT $CBLARGS $PKGS --enable-tests --only-dependencies

# List what we've got:
cabal sandbox hc-pkg list

echo "Everything installed, now to test."
for subdir in $PKGS; do 
  cd "$root/$subdir"
  # Print the individual test outputs:
  cabal test --show-details=always
done
