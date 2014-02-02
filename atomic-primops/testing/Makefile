
GHC=ghc

all: simple full

simple: hello
hello:
	$(GHC) -fforce-recomp hello.hs -o hello_regular.exe 
	$(GHC) -fforce-recomp hello.hs -o hello_threaded.exe -threaded
	$(GHC) -fforce-recomp hello.hs -o hello_prof.exe -prof
	$(GHC) -fforce-recomp hello.hs -o hello_prof_threaded.exe -prof -threaded
	./hello_regular.exe 
	./hello_threaded.exe
	./hello_prof.exe
	./hello_prof_threaded.exe

NOPROF= --disable-library-profiling --disable-executable-profiling
PROF= --enable-library-profiling --enable-executable-profiling

CBL= rm -rf dist; echo "\n\n"; cabal install --enable-tests

#================================================================================
# The --builddir approach is broken with cabal 1.16.  It works fine in
# the current cabal HEAD [2013.07.18].
# ================================================================================

# Try all combinations of profiling, optimization, and threading:
full:
	$(CBL) $(NOPROF) -fopt  -fthreaded  # --builddir=dist_reg_opt_thrd
	$(CBL) $(PROF)   -fopt  -fthreaded  # --builddir=dist_prof_opt_thrd

	$(CBL) $(NOPROF) -f-opt -fthreaded  # --builddir=dist_reg__thrd
	$(CBL) $(PROF)   -f-opt -fthreaded  # --builddir=dist_prof__thrd

	$(CBL) $(NOPROF) -fopt  -f-threaded # --builddir=dist_reg_opt_
	$(CBL) $(PROF)   -fopt  -f-threaded # --builddir=dist_prof_opt_

	$(CBL) $(NOPROF) -f-opt  -f-threaded # --builddir=dist_reg__
	$(CBL) $(PROF)   -f-opt  -f-threaded # --builddir=dist_prof__

# These shoud be redundant with the above, but we run them anyway:
	for f in `ls dist_*/build/hello-world-atomic-primops/hello-world-atomic-primops`; do ./$f; done

	tail dist_*/test/test-atomic-primops-*-test-atomic-primops.log

clean:
	rm -rf ./dist/ ./dist_*/
