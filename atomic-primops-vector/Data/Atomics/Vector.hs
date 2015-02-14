
module Data.Atomics.Vector
       ( casVectorElem, unsafeCasVectorElem
       , readVectorElem, unsafeReadVectorElem )
       where

import Data.Atomics
import Data.Vector.Mutable

{-# INLINE casVectorElem #-}
-- | Perform a compare-and-swap on a single element of a mutable vector.
casVectorElem :: IOVector a -> Int -> Ticket a -> a -> IO (Bool, Ticket a)
casVectorElem (MVector st len array) i tick elm =
  let idx = i + st
  in if i >= len
     then error $ "casVectorElem: out of bounds access to index "++show i++
                  " of IOVector of length "++show len
     else casArrayElem array idx tick elm

{-# INLINE unsafeCasVectorElem #-}
-- | Unsafe version of `casVectorElem` which is not bounds checked.
unsafeCasVectorElem :: IOVector a -> Int -> Ticket a -> a -> IO (Bool, Ticket a)
unsafeCasVectorElem (MVector st _ array) i tick elm =
  let idx = i + st
  in casArrayElem array idx tick elm

{-# INLINE readVectorElem #-}
readVectorElem :: IOVector a -> Int -> IO (Ticket a)
readVectorElem (MVector st len arr) ix =
  if ix >= len
  then error $ "readVectorElem: out of bounds access to index "++show ix++
               " of IOVector of length "++show len
  else readArrayElem arr (st + ix)

{-# INLINE unsafeReadVectorElem #-}
unsafeReadVectorElem :: IOVector a -> Int -> IO (Ticket a)
unsafeReadVectorElem (MVector st _ arr) ix = readArrayElem arr (st + ix)
