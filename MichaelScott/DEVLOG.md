
[2011.12.09] Some of the strange errors one can encounter presently
-------------------------------------------------------------------

Here are two consecutive runs of Test.hs in this directory:

    Reading sums from MVar...
    Test: internal error: PAP object entered!
	(GHC version 7.2.1 for x86_64_unknown_linux)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
    Aborted

And then:

    Test: internal error: evacuate: strange closure type -958224799
	(GHC version 7.2.1 for x86_64_unknown_linux)
	Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

That's using the following definition of pointer equality:

    ptrEq :: a -> a -> Bool
    ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1

