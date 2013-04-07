
ifeq ($(GHC),)
  GHC=`which ghc`
endif 

all:
	${GHC} -threaded -O2 -fforce-recomp Test.hs -o Test$(SUFFIX).exe

test: test74 test76

test74:
	GHC=ghc-7.4.2 make && ./Test.exe

test76:
	GHC=ghc-7.6.1 make && ./Test.exe
