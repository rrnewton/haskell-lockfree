
set -e
set -x

if [ "$JENKINS_GHC" == "" ]; then 
  echo "Must set JENKINS_GHC to, e.g. '7.6.3', to run this script."
  exit 1
fi

source $HOME/rn_jenkins_scripts/acquire_ghc.sh
which cabal
cabal --version

which -a llc || echo "No LLVM"


# Pass OPTLVL directly to cabal:
CBLARGS=" -j $OPTLVL "

if [ "$PROF" == "prof" ]; then 
  CBLARGS="$CBLARGS --enable-library-profiling --enable-executable-profiling"
else
  CBLARGS="$CBLARGS --disable-library-profiling --disable-executable-profiling"
fi

if [ "$HPC" == "hpc" ]; then 
  CBLARGS="$CBLARGS --enable-library-coverage"
else
  CBLARGS="$CBLARGS --disable-library-coverage"
fi

if [ "$THREADING" == "nothreads" ]; then 
  echo "Compiling without threading support."
  CBLARGS="$CBLARGS -f-threaded "
else
  CBLARGS="$CBLARGS -fthreaded --ghc-options=-threaded "
fi

ALLPKGS="$PKGS $NOTEST_PKGS"

cabal sandbox init

root=`pwd`
for subdir in $ALLPKGS; do 
  cd "$root/$subdir"
  cabal sandbox init --sandbox=$root/.cabal-sandbox
done
cd "$root"


# First install everything without testing:
CMDROOT="cabal install --reinstall --with-ghc=ghc-$JENKINS_GHC --force-reinstalls"
$CMDROOT $CBLARGS $ALLPKGS

# Now install the DEPENDENCIES for testing
$CMDROOT $CBLARGS $PKGS --enable-tests --only-dependencies

# List what we've got:
cabal sandbox hc-pkg list

echo "Everything installed, now to test."
for subdir in $PKGS; do 
  cd "$root/$subdir"
  # Print the individual test outputs:
  cabal test --show-details=always
done
