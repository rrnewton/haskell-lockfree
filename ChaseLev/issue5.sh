#!/bin/bash

if [ $GHC == "" ]; then
  GHC=ghc-7.6.3
fi 

# cd ChaseLev

CBLARGS="--disable-executable-profiling --disable-library-profiling --disable-documentation --force-reinstalls --with-ghc=$GHC"

cabal-1.17.0_HEAD install $CBLARGS ../AbstractDeque/ 
cabal-1.17.0_HEAD install $CBLARGS ../AtomicPrimops/ 

# This works in -O0, and fails in -O2.
# $GHC -O0 -fforce-recomp --make RegressionTests/Issue5.hs -o Issue5.exe -main-is RegressionTests.Issue5.standalone_single_CAS

$GHC -O1 -fforce-recomp --make RegressionTests/Issue5.hs -o Issue5.exe -main-is RegressionTests.Issue5.standalone_single_CAS

./Issue5.exe
