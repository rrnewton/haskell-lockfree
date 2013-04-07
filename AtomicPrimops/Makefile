
ifeq ($(GHC),)
  GHC=`which ghc`
endif 

all:
	${GHC} -c -dcmm-lint -keep-s-file cbits/primops.cmm
	${GHC} -O2 -fforce-recomp --make cbits/primops.o Test.hs -o Test$(SUFFIX).exe

onecall:
#	${GHC} -c -dcmm-lint -keep-s-file cbits/primops.cmm
	${GHC} -O2 -fforce-recomp --make cbits/primops.cmm Test.hs -o Test$(SUFFIX).exe

# /usr/local/bin/ghc -c -odir dist/build -Idist/build -Icbits -optc-O2 -package-db dist/package.conf.inplace -package-id base-4.6.0.1-2514ecbfe6573e639515d3e91d6e1f29 -package-id ghc-prim-0.3.0.0-bd1d393903577692a5ef2aa7027b71f6 -package-id primitive-0.5.0.1-4ecd316f3b2f8d062e9b576dcc9b7561 cbits/primops.cmm
# Linking...
# /usr/bin/ar -r -s dist/build/libHSatomic-primops-0.1.0.0.a dist/build/Data/Atomics.o dist/build/Data/Atomics/Internal.o dist/build/cbits/primops.o
# ar: creating archive dist/build/libHSatomic-primops-0.1.0.0.a
# /usr/bin/ld -x -arch x86_64 -r -o dist/build/HSatomic-primops-0.1.0.0.o dist/build/Data/Atomics.o dist/build/Data/Atomics/Internal.o dist/build/cbits/primops.o
# In-place registering atomic-primops-0.1.0.0...



test: test74 test76

test74:
	GHC=ghc-7.4.2 make && ./Test.exe

test76:
	GHC=ghc-7.6.1 make && ./Test.exe

test2: 
	${GHC} -c -dcmm-lint -keep-s-file cbits/primops.cmm
	${GHC} -O2 -fforce-recomp --make cbits/primops.o Test2.hs -o Test2$(SUFFIX).exe
