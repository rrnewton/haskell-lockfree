#!/bin/bash

set -o errexit
set -x

if [ "$HADDOCK" == "" ];
then HADDOCK=`which haddock`
fi

if [ "$CABAL" == "" ];
then CABAL=`which cabal`
fi

if [ "$GHC" == "" ];
then GHC=`which ghc`
fi

ALLPKG="./CAS ./AtomicPrimops ./AtomicPrimops/testing ./AbstractDeque ./MichaelScott ./ChaseLev ./MegaDeque"

# A manual form of cleaning.
for dir in $ALLPKG; do 
  rm -rf $dir/dist/
done

$CABAL install -fforce-recomp --force-reinstalls --with-ghc=$GHC $ALLPKG $*
