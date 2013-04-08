
ifeq ($(GHC),)
  GHC=`which ghc`
endif 

# ifeq ($(GHC_OPTS),)
#   GHC_OPTS= -threaded -O2
# endif 

# TODO: Cover all the GHC "Ways":

threadedO2:
	SUFFIX=_threaded GHC_OPTS="-threaded -O2" ${MAKE} plain

prof:
	SUFFIX=_prof GHC_OPTS="-prof" ${MAKE} plain

plain:
	${GHC} ${GHC_OPTS} -fforce-recomp Test.hs -o Test$(SUFFIX).exe


#------------------------------

test: test74 test76

test74:
	GHC=ghc-7.4.2 make && ./Test.exe

test76:
	GHC=ghc-7.6.1 make && ./Test.exe
