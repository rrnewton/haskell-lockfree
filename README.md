
Contents of this Repository
================================================================================

This is a multi-package repository.  The following directories
correspond to the following packages:

    CAS            - IORefCAS
    AbstractDeque  - abstract-deque
    MichaelScott   - lockfree-queue
    ChaseLev       - chaselev-deques
    MegaDeque      - mega-deque

Please see the .cabal files for descriptions of each package.  Here's
a brief summary:

  * CAS - Compare and swap operations for 
  * AbstractDeque - abstract interface for single and double ended
    queues, plus reference implementation in pure Haskell
  * MichaelScott - classic Michael & Scott algorithm for single ended
    queues
  * ChaseLev - Work-stealing "1.5" ended deques.
  * MegaDeque - a package that picks the best implementation for the
    interface constraints, which are expressed at the type level.
  

How to Test and Install
================================================================================

First, to really use compare-and-swap based data structures, you
should be using GHC 7.4.1 or later.  These libaries will be force to
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

KNOWN PROBLEMS
================================================================================

  * [2012.02.29] The tests for abstract-deque currently stack overflow, but pass
    with an increased (100M) stack size.
  
  * [2012.02.29] ChaseLev deques exhibit nondeterministic failures
    still.  Hopefully this is due to a bug in the implementation and
    not further problems in Haskell's CAS (there was a fix AFTER
    ghc-7.2.1).
  

                TODO
----------------------------------------

 * It would be nice to add a binding to TBB concurrent queues or other
   foreign implementations (for storable types).

 * Test on windows.  (It hasn't been tried.)
