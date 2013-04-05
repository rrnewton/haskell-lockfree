

[2013.04.05] {A note on the ChaseLev crashes}
=============================================

On the Mine machines this will consistently get an uninitialized
error:

   time NUMELEMS=5000000 ./dist/build/test-chaselev-deque/test-chaselev-deque -tOneBot +RTS -N4

