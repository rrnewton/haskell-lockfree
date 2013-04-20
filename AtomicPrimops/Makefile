
ifeq ($(GHC),)
  GHC=ghc
endif 

ifeq ($(CABAL),)
  CABAL=cabal-dev
endif 


INSTALL_FLAGS = --disable-library-profiling --disable-documentation
# This seems broken in cabal-dev 0.9.2
# INSTALL_FLAGS += -j

test: buildtest runtest

buildtest:
	$(CABAL) install $(INSTALL_FLAGS) --with-ghc=$(GHC) ./ ./testing/

runtest:
	./cabal-dev/bin/test-atomic-primops || test-atomic-primops

#--------------------------------------------------------------------------

all: test76 test74 test72 

# Not working:
test70:
	GHC=ghc-7.0.4 make test

test72:
	GHC=ghc-7.2.1 make test

test74:
	GHC=ghc-7.4.2 make test

test76:
	GHC=ghc-7.6.2 make test
