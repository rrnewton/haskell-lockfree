{-# LANGUAGE TemplateHaskell #-}

-- | Test the invocation of the GHCi bytecode intepreter with atomic-primops.

module Main where

import Data.Atomics -- import needed to test whether ghci linking error occurs
import TemplateHaskellSplices (tmap)
import Test.Framework  (defaultMain)
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain
 [
   ----------------------------------------
   testCase "Template_Haskell_invocation" $ do
     putStrLn "Attempting Template Haskell implementation of map operation"
     print $ $(tmap 3 4) (+ 1) (1,2,3,4) -- comment out for compilation to succeed
   ----------------------------------------
 ]
