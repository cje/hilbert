{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE DeriveGeneric #-} 
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Data.Algorithm.Hilbert.Types
(
-- * Types for pointToIndex 
--  PrecisionNum(value,precision) 
  PrecisionNum(..) 
, minPrecision 
, mkPrecisionNum 
, precisionRequired
, floatingPlus 
, floatingTwoExp 
, boolToNum 
, shiftRA
, shiftLA 
)
 where

import qualified Data.Bits as DB 
import Data.Maybe
--import Control.Applicative
import Control.Exception
import Control.DeepSeq  
import Control.DeepSeq.Generics 
import GHC.Generics 

data PrecisionNum    = PrecisionNum { value :: Integer
                                    , precision :: Int} deriving (Show, Generic ) 

instance NFData PrecisionNum where 
   rnf = genericRnf  

-- | Convert an integer to a list of Bools corresponding to its binary 
-- representation.  
-- For example, to represent the integer 3 in 4 bits: 
--
-- > numToBool (3::Int) 4
-- > = [True,True,False,False]
numToBool :: PrecisionNum -> [Bool]  
numToBool i = map (DB.testBit i) [0..topmostBit]
                   where topmostBit = fromIntegral $ precision i - 1  

-- | Convert a list of Bool representation of an integer 
-- to an integer. 
-- For example, to obtain an integer from its binary representation:  
-- Assumes that the precision of the returned value should be equal to 
-- the number of bits that were provided. 
-- > boolToNum [False, True, True, True] 
-- > = 14
--

boolToNum ::  [Bool] -> Maybe PrecisionNum 
boolToNum b =  mkPrecisionNum num bits
                where num = sum (zipWith makeNumber b allTheNumbers)
                      allTheNumbers = [0 .. ] :: [Int]
                      bits = length b :: Int

makeNumber :: forall a b. (Enum a, Integral b) => a ->  b -> Integer
makeNumber x y = (fromIntegral . fromEnum) x  * 2^y



shiftLA ::  (Integral a) => PrecisionNum -> a -> PrecisionNum  
shiftLA n amount =  let b = numToBool n  
                        shifted = replicate (fromIntegral amount) False ++ b
                    in 
                        fromJust $ boolToNum shifted  

-- shiftRA is generally expected to reduce precision. 
-- The exception is where a single bit is shifted right. 
-- In that case, we want to return zero, with a single bit of precision.
-- Accordingly, we lower bound the value. 
shiftRA :: (Integral a) =>  PrecisionNum -> a -> PrecisionNum 
shiftRA n amount =  let b = numToBool n  
                        shifted = drop (fromIntegral amount) b
                        lowerBounded = max [False] shifted 
                    in 
                        fromJust $ boolToNum lowerBounded  


bitSizeA :: PrecisionNum -> Int 
bitSizeA = precision 

leftNBits :: Int -> PrecisionNum -> Integer 
leftNBits n (PrecisionNum v p) = v DB..&. ((2^p - 1) - (2^(p-n) - 1)) 

rightNBits :: Int -> PrecisionNum -> Integer  
rightNBits n (PrecisionNum v _) = v DB..&. (2^n - 1) 

instance Bounded (PrecisionNum) where 
     minBound = PrecisionNum 0 1  
     maxBound = PrecisionNum largestInt (maxBound :: Int)  
                         where largestInt = 2^(maxBound :: Int) 

instance DB.FiniteBits PrecisionNum where 
     finiteBitSize  = bitSizeA   
     

instance DB.Bits (PrecisionNum) where 
     (.|.) a b = PrecisionNum { value = value a DB..|. value b
                              , precision = max (precision a) (precision b) 
                             } 
     (.&.) a b = PrecisionNum { value = value a DB..&. value b
                              , precision = max (precision a) (precision b) 
                              } 
     xor a b =   PrecisionNum { value = value a `DB.xor` value b
                              , precision = max (precision a) (precision b) 
                              }
     shiftR = shiftRA  
     shiftL = shiftLA    
     rotateR v amount = PrecisionNum { value = -- Make the left right, ...  
                                       left `DB.shiftR` amount'  
                                       DB..|. 
                                               -- And the right left. 
                                       right `DB.shiftL` balance  
                                     , precision = precision v  
                                     } 
                             where  left = leftNBits balance v   
                                    right = rightNBits amount' v  
                                    balance = DB.finiteBitSize v - amount' 
                                    amount' = amount `mod` (fromJust . DB.bitSizeMaybe) v
     rotateL v amount  = PrecisionNum {  value = 
                                                -- Make the right left, ... 
                                         right `DB.shiftL` amount'  
                                         DB..|. 
                                                -- And the left right.
                                         left `DB.shiftR` balance  
                                       , precision = precision v  
                                      }
                             where  left = leftNBits amount' v 
                                    right = rightNBits balance v 
                                    balance = (fromJust . DB.bitSizeMaybe) v - amount' 
                                    amount' = amount `mod` (fromJust . DB.bitSizeMaybe) v   
     bitSizeMaybe v = Just $ bitSizeA v 
     testBit (PrecisionNum v _) index = DB.testBit v index  
     complement (PrecisionNum v p) = PrecisionNum { value = DB.complement v
                                                   , precision = p } 
     bit i = PrecisionNum { value = DB.bit i,    
                            precision =  i      
                           } 

     isSigned (PrecisionNum v _) = DB.isSigned v 
 
     popCount (PrecisionNum v _) = DB.popCount v            

instance Enum (PrecisionNum) where
   succ (PrecisionNum v p) = PrecisionNum {value = succ v, 
                                           precision = fromIntegral (max (precisionRequired v) (fromIntegral p))}  
   pred (PrecisionNum v p) = PrecisionNum {value = pred v, 
                                           precision = p} 
   toEnum i                = PrecisionNum { value = fromIntegral i, 
                                           precision = fromIntegral (precisionRequired (fromIntegral i))  
                                           }
   fromEnum (PrecisionNum v _) = fromIntegral v 
     
  
instance Integral (PrecisionNum) where  
    quot (PrecisionNum v1 p1) (PrecisionNum v2 p2) = PrecisionNum { value = quot v1 v2, 
                                                                   precision = max p1 p2
                                                                 }  
    rem (PrecisionNum v1 p1) (PrecisionNum v2 p2) = PrecisionNum { value = rem v1 v2, 
                                                                   precision = max p1 p2
                                                                 }  
    div (PrecisionNum v1 p1) (PrecisionNum v2 p2) = PrecisionNum { value = div v1 v2, 
                                                                   precision = max p1 p2
                                                                 }  
    mod (PrecisionNum v1 p1) (PrecisionNum v2 p2) = PrecisionNum { value = mod v1 v2, 
                                                                   precision = max p1 p2
                                                                 }  
    quotRem a1 a2 = (quot a1 a2, rem a1 a2) 
    divMod a1 a2 = (div a1 a2, mod a1 a2)  
    toInteger (PrecisionNum v1 _)  = toInteger v1 

instance Real (PrecisionNum) where 
       toRational (PrecisionNum v _) = toRational v 

instance  Eq (PrecisionNum) where
      (==) (PrecisionNum v1 _) (PrecisionNum v2 _) = v1 == v2 

instance Ord (PrecisionNum) where
      (compare)  (PrecisionNum v1 _) (PrecisionNum v2 _) = compare v1  v2 
      (>)  (PrecisionNum v1 _) (PrecisionNum v2 _) = v1 > v2 
      (<)  (PrecisionNum v1 _) (PrecisionNum v2 _) = v1 < v2 
      (>=)  (PrecisionNum v1 _) (PrecisionNum v2 _) = v1 >= v2 
      (<=)  (PrecisionNum v1 _) (PrecisionNum v2 _) = v1 <= v2 
      max (PrecisionNum v1 p1) (PrecisionNum v2 p2) = if v1 > v2 then 
                                                         PrecisionNum v1 p1  
                                                      else 
                                                         PrecisionNum v2 p2  
      min (PrecisionNum v1 p1) (PrecisionNum v2 p2) = if v1 < v2 then 
                                                         PrecisionNum v1 p1  
                                                      else 
                                                         PrecisionNum v2 p2  

floatingPlus :: PrecisionNum -> PrecisionNum -> PrecisionNum
floatingPlus a b = minPrecision (value a + value b)   

floatingTwoExp :: PrecisionNum -> PrecisionNum
floatingTwoExp a = minPrecision $ (2::Integer) ^ value a 

instance Num PrecisionNum where
        (+) a b = fromJust $ mkPrecisionNum (value a + value b) ( max (precision a) (precision b))  
        (-) a b = fromJust $ mkPrecisionNum (value a - value b) ( max (precision a) (precision b))  
        (*) a b = fromJust $ mkPrecisionNum (value a * value b) ( max (precisionRequired (value a * value b)) ( max (precision a) (precision b)))
        fromInteger a  = PrecisionNum {  
                              value =   b  
                             , precision = precisionRequired b
                             } 
                             where b = fromIntegral a  
        abs a   = PrecisionNum { value = abs (value a) 
                               , precision = precision a 
                               } 
        negate a   = PrecisionNum { value = negate (value a) 
                               , precision = precision a 
                               } 
        signum a = PrecisionNum { value = signum (value a) 
                                , precision = precision a
                               }  


minPrecision :: (Integral u) => u -> PrecisionNum
minPrecision x = assert (x >= 0) 
                 fromJust $ mkPrecisionNum j (precisionRequired j)
                         where j = fromIntegral x 



mkPrecisionNum :: (Integral a, Integral b) => a -> b -> Maybe PrecisionNum  
mkPrecisionNum v p = if pn >= precisionRequired vn then  
                        Just PrecisionNum { value = vn, precision = pn }  
                     else 
                        Nothing 
                     where vn = fromIntegral v 
                           pn = fromIntegral p 

-- FIXME.  This function is slow, and called frequently.  Re-implement.    
-- eg: precisionRequired i = ceiling $ logBase 2 (fromIntegral i) 
precisionRequired :: Integer -> Int 
precisionRequired i = case i of
                            0  ->         1   
                            1  ->         1   
                            _  ->  1 + precisionRequired (i `DB.shiftR` 1)  
 
