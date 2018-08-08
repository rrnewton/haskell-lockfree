#! /bin/bash

cat stack.yaml | grep -v resolver > stack-${STACK_RESOLVER}.yaml
echo "resolver: ${STACK_RESOLVER}" >> stack-${STACK_RESOLVER}.yaml
rm -f stack.yaml
