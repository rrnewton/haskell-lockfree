## 0.8.1.1 [2017.12.10]
* Bundle `testing/Fetch.hs` with the package tarball

## 0.8.1
* Simplify `Setup.hs` to support `Cabal-2.0`/GHC 8.2
* Properly link `store_load_barrier` and friends against the GHC RTS on Windows
  when using GHC 8.2 or later

## 0.8.0.4
* Internal changes to support forthcoming GHC 8.0

## 0.8
* Implements additional fetch primops available in GHC 7.10

## 0.7
* This release adds support for GHC 7.10 and its expanded library of (now inline) primops.

## 0.6.1
* This is a good version to use for GHC 7.8.3.  It includes portability and bug fixes
  and adds atomicModifyIORefCAS.

## 0.6.0.5
* fix for GHC 7.8

## 0.6.0.1
* minor ghc 7.8 fix

## 0.6
* add atomicModifyIORefCAS, and bump due to prev bugfixes

## 0.5.0.2
* IMPORTANT BUGFIXES - don't use earlier versions.  They have been marked deprecated.

## 0.5
* Nix Data.Atomics.Counter.Foreign and the bits-atomic dependency.

## 0.4.1
* Add advance support for GHC 7.8

## 0.4
* Further internal changes, duplicate 'cas' routine well as barriers.
* Add `fetchAddByteArrayInt`
* Add an `Unboxed` counter variant that uses movable "ByteArray"s on the GHC heap.

## 0.3
* Major internal change.  Duplicate the barrier code from the GHC RTS and thus
  enable support for executables that are NOT built with '-threaded'.

## 0.2.2.1
* Minor, add warning.

## 0.2.2
* Add more counters

## 0.2
* Critical bugfix and add Counter.

## 0.1.0.2
* disable profiling

## 0.1.0.0
* initial release
