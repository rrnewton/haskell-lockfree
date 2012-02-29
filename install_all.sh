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


function doall () {
  CMD=$1
  (cd CAS;           $CMD)
  (cd AbstractDeque; $CMD)
  (cd MichaelScott;  $CMD)
  (cd ChaseLev;      $CMD)
  (cd MegaDeque;     $CMD)
}

# doall "cabal haddock --with-haddock=$HADDOCK"
# doall "cabal install --haddock"
doall "$CABAL install --with-ghc=$GHC $*"
