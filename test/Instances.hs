{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import System.Random
import Test.QuickCheck
import Test.QuickCheck.Modifiers 
import Data.Algorithm.Hilbert.Types 
import Control.Applicative ((<$>)) 
import Data.Maybe (fromJust) 

instance Arbitrary PrecisionNum where
   arbitrary = do Positive v <- arbitrary 
                  return $ minPrecision (v :: Integer)  

instance Random PrecisionNum where 
  random gen = (fromJust $ mkPrecisionNum (n::Int) (precisionRequired (fromIntegral n)), nGen)  
                   where (n, nGen) = System.Random.randomR (minBound :: Int, maxBound :: Int) gen 
  randomR (a, b) gen =  (fromJust $ mkPrecisionNum (n::Int) (precisionRequired (fromIntegral n)), nGen)  
                   where (n, nGen) = System.Random.randomR ( fromIntegral (value a), fromIntegral (value b))  gen 



