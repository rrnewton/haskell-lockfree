
Build Status and unit tests
===========================

 * Jenkins: [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=Haskell-LockFree_primops)](http://tester-lin.soic.indiana.edu:8080/job/Haskell-LockFree_primops/) -- Basic primops only, i.e. `atomic-primops` package:
 * Jenkins: [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=Haskell-LockFree_dataStructs)](http://tester-lin.soic.indiana.edu:8080/job/Haskell-LockFree_dataStructs) -- all Queue and Deque data structures in this package.
 * Travis: [![Build Status](https://travis-ci.org/rrnewton/haskell-lockfree.svg?branch=master)](https://travis-ci.org/rrnewton/haskell-lockfree) -- combined build&test for all packages in the repo.

Contents of this Repository
================================================================================

This is a multi-package repository.  The following directories
each correspond to exactly one cabal package:

 * [abstract-deque]: AbstractDeque - abstract interface for single and
    double ended queues, plus reference implementation in pure Haskell
 * [lockefree-queue]: classic Michael & Scott algorithm for single ended queues
 * [chaselev-deque]: work-stealing "1.5" ended deques.
 * [mega-deque]: a package that picks the best implementation for the
    interface constraints, which are expressed at the type level.
 * [atomic-primops]: *safe* CAS/FAA (compare-and-swap/fetch-and-add) on various kinds of mutable locations
 * [atomic-primops-foreign]: Add on package that provides an FFI based
   implementation of counters.

Please see the .cabal files for more detailed descriptions of each package.


How to Test and Install
================================================================================

First, to use compare-and-swap based data structures, you should be
using GHC 7.4.1 or later.  Some of these libaries will be forced to
"fake it" on earlier versions of GHC.

You can install all of the packages in your user's .cabal directory
with the following command:

    ./install_all.sh

You may also want to build the tests at the same time:

    ./install_all.sh --enable-tests

There are currently [2012.02.29] some problems with cabal failing to
automatically install dependencies for *tests* (as opposed to
libraries).  You may have to manually install some packages via cabal
(e.g. `test-framework-HUnit`).

If you take a look at that `install_all.sh` script, you'll notice you
can also configure which executables for `cabal` and `ghc` it uses,
for example:

    GHC=ghc-7.4.1 CABAL=cabal-0.10.2 ./install_all.sh --enable-tests

Next, you can run the tests like this:

    ./MichaelScott/dist/build/test-lockfree-queue/test-lockfree-queue

That test should complete successfully with a zero exit code.

Note that if you have trouble building test-suites through cabal, you
can build them manually with GHC using a command like the following:

    cd MichaelScott/
    ghc-7.4.1 -O2 -threaded -rtsopts Test.hs -o Test.exe

Building with Profiling for debugging:
----------------------------------------


    ghc-7.4.1 -prof -osuf=o_p -O2 -threaded -rtsopts Test.hs -o Test.exe

You might have to reinstall some of the dependencies with profiling
enabled:

    cabal install -p hostname xml regex-base regex-posix ansi-terminal ansi-wl-pprint test-framework test-framework-hunit --reinstall

Reinstalling with profiling can be REALLY annoying once the libraries
are already installed.  For example, if you forget a dependency above
it will complete most of the compile, giving you the "profiling
version not available" error only later on, resulting in a quadratic
compilation process as you reinstall, add one more dep, reinstall,
repeat.



KNOWN PROBLEMS
================================================================================

                TODO
----------------------------------------

 * It would be nice to add a binding to TBB concurrent queues or other
   foreign implementations (for storable types).

 * Test on windows.  (It hasn't been tried.)
