#!/bin/bash

# Test the data structures built on top of the core atomic-primops package.

NOTEST_PKGS="atomic-primops/ atomic-primops-foreign/"
PKGS="abstract-deque/ abstract-deque-tests/ lockfree-queue/ chaselev-deque/ mega-deque/"
TESTPKGS="$PKGS"

source .jenkins_common.sh
