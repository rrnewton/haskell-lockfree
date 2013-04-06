
ifeq ($(GHC),)
  GHC=`which ghc`
endif 

all:
	${GHC} -c -dcmm-lint -keep-s-file cbits/primops.cmm
	${GHC} -O2 -fforce-recomp --make cbits/primops.o Test.hs -o Test$(SUFFIX).exe

test: test74 test76

test74:
	GHC=ghc-7.4.2 make && ./Test.exe

test76:
	GHC=ghc-7.6.1 make && ./Test.exe

test2: 
	${GHC} -c -dcmm-lint -keep-s-file cbits/primops.cmm
	${GHC} -O2 -fforce-recomp --make cbits/primops.o Test2.hs -o Test2$(SUFFIX).exe
