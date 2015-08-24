#!/bin/bash

set -xe

if [ "$STACKVER" == "" ]; then
# Legacy cabal version; soon to be deprecated:

    cabal --version
    echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
    which -a ghc
    ghc --version

    cabal update # Can put a retry here...

    # This is a hack to make Travis happy because it doesn't install happy/alex by default
    # cabal install -j happy alex

    # And now we install the main packages:
    PKGS="./atomic-primops ./atomic-primops-foreign ./abstract-deque/ ./abstract-deque-tests/ ./lockfree-queue/ ./chaselev-deque/ ./mega-deque"
    cabal install -j $PKGS

    # Now enable benchmarks and tests and add the extra dependencies:
    cabal install -j --only-dependencies --enable-tests --enable-benchmarks $PKGS
else
    cat stack.yaml | grep -v resolver > stack-${STACK_RESOLVER}.yaml
    echo "resolver: ${STACK_RESOLVER}" >> stack-${STACK_RESOLVER}.yaml
    rm -f stack.yaml # Just to make sure.

    URL="https://github.com/commercialhaskell/stack/releases/download/v"${STACKVER}"/stack-${STACKVER}-x86_64-linux.gz"
    # travis_retry:
    curl -L $URL | gunzip > ~/.local/bin/stack
    chmod a+x ~/.local/bin/stack

    # In this mode we just grab the latest from hackage:
    if [ ${STACK_RESOLVER%-*} = "ghc" ]; then
        which -a ghc
        ghc --version
        stack --resolver=${STACK_RESOLVER} solver --modify-stack-yaml
    fi

    # Sweet and simple:
    stack setup --no-terminal
    stack test --only-snapshot --no-terminal
fi
