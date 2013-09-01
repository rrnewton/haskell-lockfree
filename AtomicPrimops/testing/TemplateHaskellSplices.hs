{-# LANGUAGE TemplateHaskell,
             RankNTypes       #-}

-- | TH splices used in atommic-primops tests.
--   Splices defined in own module for technical reasons.

module TemplateHaskellSplices where

import Language.Haskell.TH
import Control.Monad (replicateM)

tmap :: forall a. (Enum a, Eq a, Num a)
        => a -> Int -> Q Exp
tmap i n = do
    f <- newName "f"
    as <- replicateM n (newName "a")
    lamE [varP f, tupP (map varP as)] $
        tupE [  if i == i'
                    then [| $(varE f) $a |]
                    else a
               | (a,i') <- map varE as `zip` [1..] ]
