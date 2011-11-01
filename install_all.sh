#!/bin/bash

set -o errexit

if [ "$HADDOCK" == "" ];
then HADDOCK=`which haddock`
fi

function doall () {
  CMD=$1
  (cd CAS;           $CMD)
  (cd MichaelScott;  $CMD)
  # (cd ChaseLev;  $CMD)
}

doall "cabal haddock --with-haddock=$HADDOCK"
doall "cabal install"

