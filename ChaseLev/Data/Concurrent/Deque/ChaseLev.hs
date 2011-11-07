{-# LANGUAGE FlexibleInstances #-}

-- | Chase-Lev work stealing Deques
-- 
-- This module only provides instances that adapt Edward Kmett's Deque
-- library to the 'Control.Concurrent.Deque.Class' interface.
module Data.Concurrent.Deque.ChaseLev () where

import qualified Data.Concurrent.Deque.ReactorDeque as R
import Data.Concurrent.Deque.Class
-- import Data.Vector.Unboxed.Mutable
import Data.Array.IO

-- | For an explanation of the implementation, see \"Dynamic Circular Work-Stealing Deque\" 
-- by David Chase and Yossi Lev of Sun Microsystems.

instance DequeClass (R.Deque IOArray) where 
  newQ  = R.empty 
  pushL q v = R.push v q 
  tryPopR q = do x <- R.steal q
		 case x of 
		   R.Empty -> return Nothing 
		   R.Abort -> return Nothing
		   R.Stolen e -> return (Just e)

instance PopL (R.Deque IOArray) where 
   tryPopL = R.pop

t = do q <- (newQ :: IO (R.Deque IOArray Int)); pushL q 3; tryPopL q
