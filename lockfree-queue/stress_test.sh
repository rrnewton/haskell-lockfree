#!/bin/bash

# Example of how to pump up the number of elements for more intensive testing.

ghc -O2 --make Test.hs -o Test.exe -rtsopts -threaded 

time NUMELEMS=500000 ./Test.exe +RTS -N

