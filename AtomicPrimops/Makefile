#--------------------------------------------------------------------------------
# INSTRUCTIONS:
#
#  This builds the tests with several GHC versions and with or without profiling.
# 
#--------------------------------------------------------------------------------

ifeq ($(GHC),)
  GHC=ghc
endif 

ifeq ($(CABAL),)
  CABAL=cabal-dev
endif 

ifeq ($(DEST),)
  DEST=cat
endif 

ifeq ($(INSTALL_FLAGS),)
  INSTALL_FLAGS = --disable-library-profiling --disable-documentation
endif 

ifeq ($(CABAL),cabal-dev)
  DEPSINSTALL= install-deps -s cabal-dev/$(GHC)
  EXEC= ./cabal-dev/$(GHC)/bin/test-atomic-primops_$(GHC)
  INSTALL_FLAGS += -s cabal-dev/$(GHC)
else
  DEPSINSTALL= install --only-dependencies -j
#  EXEC= ./dist/build/test-atomic-primops/test-atomic-primops_$(GHC)
  EXEC=$(HOME)/.cabal/bin/test-atomic-primops_$(GHC)
endif

# Ok, --enable-executable-profiling seems BROKEN [2013.04.20].  It's simply not passing -prof to ghc.
# INSTALL_FLAGS = --enable-library-profiling --enable-executable-profiling
# INSTALL_FLAGS = --disable-library-profiling --disable-documentation
# This seems broken in cabal-dev 0.9.2
# INSTALL_FLAGS += -j

test: buildtest runtest

buildtest:
#       First install deps WITH profiling:
	$(CABAL) $(DEPSINSTALL) --enable-library-profiling --disable-documentation --with-ghc=$(GHC) ./ ./testing/
#       Install the lib and executable with the specified mode:
	$(CABAL) install $(INSTALL_FLAGS) --disable-documentation --with-ghc=$(GHC) --program-suffix=_$(GHC) ./ ./testing/

runtest:
	$(EXEC) +RTS -N -T -RTS

#--------------------------------------------------------------------------

# 7.0 and 7.2 having problems...
all: test76 test74 

hydra:
#	DEST=hydra-head ${MAKE} -j all 
#	(hydra-view &); (DEST=hydra-head ${MAKE} -j all &); fg %1
	(DEST=hydra-head ${MAKE} -j all) | hydra-view


test70:
	GHC=ghc-7.0.4 ${MAKE} test 2>&1 | ${DEST}

test72:
	GHC=ghc-7.2.2 ${MAKE} test 2>&1 | ${DEST}

test74:
	GHC=ghc-7.4.2 ${MAKE} test 2>&1 | ${DEST}

test76:
	GHC=ghc-7.6.2 ${MAKE} test 2>&1 | ${DEST}

prof70:
	GHC=ghc-7.0.4 INSTALL_FLAGS="-v --enable-library-profiling --ghc-options=-prof" ${MAKE} test 2>&1 | ${DEST}

prof72:
	GHC=ghc-7.2.2 INSTALL_FLAGS="-v --enable-library-profiling --ghc-options=-prof" ${MAKE} test 2>&1 | ${DEST}

prof74:
	GHC=ghc-7.4.2 INSTALL_FLAGS="-v --enable-library-profiling --ghc-options=-prof" ${MAKE} test 2>&1 | ${DEST}

prof76:
	GHC=ghc-7.6.2 INSTALL_FLAGS="-v --enable-library-profiling  --ghc-options=-prof" ${MAKE} test 2>&1 | ${DEST}


global70:
	CABAL=cabal ${MAKE} test70

global72:
	CABAL=cabal ${MAKE} test72

global74:
	CABAL=cabal ${MAKE} test74

global76:
	CABAL=cabal ${MAKE} test76
