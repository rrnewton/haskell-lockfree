#!/bin/bash

# Test only the core atomic-primops packages.

NOTEST_PKGS=""
PKGS="atomic-primops/ atomic-primops/testing/ atomic-primops-foreign/"

source .jenkins_common.sh
