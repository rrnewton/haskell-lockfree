#!/bin/bash

set -e

if [ "$GHC" == "" ]; then
  GHC=ghc-7.6.3
fi 

# cd ChaseLev

CBLARGS="--disable-executable-profiling --disable-library-profiling --disable-documentation --force-reinstalls --with-ghc=$GHC"

cabal-1.17.0_HEAD install $CBLARGS ../AbstractDeque/ 
cabal-1.17.0_HEAD install $CBLARGS ../AtomicPrimops/ 

# This works in -O0, and fails in -O2.
# $GHC -O0 -fforce-recomp --make RegressionTests/Issue5.hs -o Issue5.exe -main-is RegressionTests.Issue5.standalone_single_CAS


# OPT="-O2 -threaded -rtsopts"
OPT="-O1 -threaded"
DBG="-keep-tmp-files -dsuppress-module-prefixes -ddump-to-file -ddump-core-stats -ddump-simpl-stats -dcore-lint -dcmm-lint -ddump-ds -ddump-simpl -ddump-stg -ddump-asm -ddump-bcos -ddump-cmm -ddump-opt-cmm -ddump-inlinings -fforce-recomp"

#$GHC $OPT $DBG --make RegressionTests/Issue5.hs -o Issue5.exe -main-is RegressionTests.Issue5.standalone_pushPop

$GHC $OPT $DBG -DDEBUGCL --make RegressionTests/Issue5B.hs -o Issue5B.exe -main-is RegressionTests.Issue5B.standalone_pushPop

./Issue5B.exe
