
ifeq ($(GHC),)
  GHC=`which ghc`
endif 


test:
	cabal-dev install --disable-library-profiling --disable-documentation --with-ghc=$(GHC)
	./cabal-dev/bin/test-atomic-primops

all: test74 test76

test74:
	GHC=ghc-7.4.2 make && ./Test.exe

test76:
	GHC=ghc-7.6.2 make && ./Test.exe
