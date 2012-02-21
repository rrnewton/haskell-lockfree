#!/bin/bash

set -o errexit

if [ "$HADDOCK" == "" ];
then HADDOCK=`which haddock`
fi

if [ "$CABAL" == "" ];
then CABAL=`which cabal`
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
doall "$CABAL install $*"
