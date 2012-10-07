
#GHC=ghc-7.6.1
GHC=ghc-7.4.2

all:
	${GHC} -c -dcmm-lint cbits/primops.cmm
	${GHC} -O2 -fforce-recomp --make cbits/primops.o Test.hs -o Test.exe
