{-# LANGUAGE OverlappingInstances, MultiParamTypeClasses #-}

-- | A type class capturing mutable storage cells that support CAS
--   operations in the IO monad.

module Data.CAS.Class where

-- | It would be nice to use an associated type family with this class
--   (for casref), but that would preclude overlapping instances.
class CASable casref a where 
  newCASable   :: a -> IO (casref a)
  readCASable  :: casref a -> IO a 
  writeCASable :: casref a -> a -> IO ()
  cas          :: casref a -> a -> a -> IO (Bool,a)

