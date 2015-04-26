{-# LANGUAGE RankNTypes #-} 

-- | The following module implements  
-- /Compact Hilbert Indices - Technical Report -- CS-2006-07/ 
-- by Chris Hamilton.  At the time of writing this comment, the document is
-- found at:
-- <https://www.cs.dal.ca/sites/default/files/technical_reports/CS-2006-07.pdf> 
-- 

module Data.Algorithm.Hilbert.Utility (
-- * Gray code  
  grayCode
, grayCodeInverse
-- * Bit operations  
, trailingSetBits 

, convertPointToHypercube
, convertInteger 
, numToBool
, boolToNum
, entryPoint
, exitPoint 
, direction
, inverseTransform 
, transform 
, toParameters 
)
 where  
import Control.Exception (assert) 
import Data.Algorithm.Hilbert.Types
import Data.Bits 
import Data.List 
import Data.Maybe

-- | Generate the ith binary reflected graycode given i.  See Theorem 2.1
-- of Hamilton. 
grayCode :: PrecisionNum -> PrecisionNum  
grayCode (PrecisionNum v p) = PrecisionNum { value = v `xor` shifted, precision = p } 
                                    where shifted = v `shiftR` 1 

-- | Generate i given the ith binary reflected graycode. 
grayCodeInverse :: PrecisionNum -> PrecisionNum 
grayCodeInverse g |       g == 0                  =  g  
                  | otherwise                     =  g `xor` grayCodeInverse shifted
                                                         where shifted = g `shiftR` 1

-- | Lemma 2.3 of Hamilton's paper deals with the dimension of the gray
-- code change at any point in the sequence it depends on the number of
-- bits that are set in the item of the  sequence just before that  point.
-- For example, in the sequence below, the value in the trailingSetBits
-- column for each entry predicts the position of the bracketed,
-- changed number in the following row. 
--
--
--  >   i   | grayCode i | trailingSetBits i   
--  >   ------------------------------------ 
--  >   0   | 000        |  0 
--  >   1   | 00(1)      |  1 
--  >   2   | 0(1)1      |  0  
--  >   3   | 01(0)      |  2 
--  >   4   | (1)10      |  0   

--This is also referred to as the inter sub-hypercube dimension, g(i) 
trailingSetBits :: PrecisionNum -> PrecisionNum  
trailingSetBits val | testBit val 0 = floatingPlus 1  (trailingSetBits (val `shiftR` 1)) 
                    | otherwise     = 0  

-- | Calculate entry point for hypercube based on index.  Referred to as
--  e(i) in Hamilton.   
-- Is the return type here just a [Hypercube a]?  
entryPoint ::  PrecisionNum -> PrecisionNum
entryPoint i | i == 0                  =  i  -- i is Zero.  
             | otherwise               =  let r = grayCode $ clearBit (i - 1) 0
                                             in 
                                          -- Must not change precision. 
                                             assert (precision r == precision i) r  
                                              
-- | Calculate exit point for hypercube based on index.  Referred to as
--  f(i) in Hamilton.  
exitPoint ::  PrecisionNum -> PrecisionNum -> PrecisionNum
exitPoint i n = entryPoint i `xor` setBit 0 (fromIntegral (direction i n)) -- From Lemma 2.11 

--
-- | Lemma 2.8 Calculate the direction in the hypercube.  Referred to as the
-- intra sub-hypercube dimension, d(i)  Note that n doesn't come into play
-- as long as i < 2^n - 1 - which means that we really need to consider
-- whether its worth passing n, or just limiting i in the beginning. 
--
direction :: PrecisionNum  -> PrecisionNum -> PrecisionNum  
direction i n | i == 0 = 0                -- Zero  
              | even i = trailingSetBits (i-1)  `mod` fromIntegral n    -- Even  
              | otherwise  = trailingSetBits i `mod` fromIntegral n     -- Odd  


-- |  See Section 2.1.3 of Hamilton, 'Rotations and Reflections', which 
-- describes transformation of entry and exit points.   
-- The rotation in this function needs to be performed with a bit size 
-- equal to the dimension of the problem.  
--  assert(b < 2^dimension && e < 2^dimension)  
transform ::  PrecisionNum ->  PrecisionNum -> PrecisionNum -> PrecisionNum
transform e d b =  (b `xor` e) `rotateR` amount 
                      where amount = fromIntegral (floatingPlus d 1)  
--transform e d b dimension = rotateRmodn (b `xor` e) (d+1) dimension 

-- |  See Section 2.1.3 of Hamilton, 'Rotations and Reflections', which 
-- describes transformation of entry and exit points.  

-- inverseTransform returns a number in the interval (0 .. 2^order-1)  
inverseTransform ::  PrecisionNum -> PrecisionNum -> PrecisionNum -> PrecisionNum
inverseTransform e d b =  (b `rotateL` amount) `xor` e  
                      where amount = fromIntegral (floatingPlus d 1)  

-- | convertPointToHypercube relates to s 2.1.4 'Algorithms' of Hamilton, 
-- and specifically the transformation of p (a vector of points) 
-- into l, by a formula
-- l_(m-1) = [bit (p_(n-1), m - 1) ... bit (p_0, m - 1)] 
--
-- An example is perhaps helpful in understanding what it does.  
-- 
-- Given a list of n integers: [1, 2, 3],  their binary representation is 
-- 
-- > | 0 0 1 | 
-- > | 0 1 0 | 
-- > | 0 1 1 | 
--
-- Transpose the above representation, to form a row from each column,
--
-- > | 0 0 0 | 
-- > | 0 1 1 | 
-- > | 1 0 1 | 
--
-- The transposed representation is converted back to a list of 
-- integers [ 0, 3, 5] 
-- Eg: 
--
-- > convertPointToHypercube ([1, 2, 3]) 3 
-- > = [0, 3, 5] 
--
-- Each integer in the list successively identifies a subhypercube 
-- in which our original point exists.   That is, to calculate the 
-- Hilbert index of the point [1, 2, 3], we traverse subhypercubes 
-- identified by [0, 3, 5] 
--
-- Each subhypercube co-ordinate is large enough to uniquely identify 
-- any vertex.  This depends only on the dimension 
-- When convertPointToHypercube is called from pointToIndex, it is  
-- passed in the Hilbert curve order as the number of bits, 
-- and a list representing a point on the curve as the vector p.  
-- 
-- In that situation it will return a list having a length equal to the 
-- /order/ of the Hilbert curve, in which each element of the list is 
-- representable in less than 2^dimension.  
-- 
--  
convertPointToHypercube :: [PrecisionNum] -> Maybe [PrecisionNum]  
convertPointToHypercube point = mapM boolToNum (f point) 
                                       where f =  reverse . transpose . reverse . map numToBool 

-- | Convert an integer to a list of Bools corresponding to its binary 
-- representation.  
-- For example, to represent the integer 3 in 4 bits: 
--
-- > numToBool (3::Int) 4
-- > = [True,True,False,False]
numToBool :: PrecisionNum -> [Bool]  
numToBool i = map (testBit i) [0..topmostBit]
                 where topmostBit = fromIntegral $ precision i -1 

-- Given a list of n integers: [1, 2, 3],  their binary representation is 
-- > | 0 0 1 | 
-- > | 0 1 0 | 
-- > | 0 1 1 | 

convertInteger :: PrecisionNum -> Int -> Int -> Maybe [PrecisionNum] 
convertInteger i bitWidth chunks = sequence $ convertInteger' intAsBits bitWidth  
                              where  targetLength = bitWidth * chunks  
                                     correctedValue = fromJust $ mkPrecisionNum (value i) targetLength 
                                     intAsBits =  numToBool correctedValue :: [Bool] 

convertInteger' :: (Integral b) =>  [Bool] -> b -> [Maybe PrecisionNum]  
convertInteger' []   _      = [] 
convertInteger' bitList stride = 
                     let 
                         (this, rest) = splitAt (fromIntegral stride) bitList 
                     in 
                         convertInteger' rest stride  ++ [boolToNum this]  

checkOrder :: (Integral a) => Int -> Int -> [a] -> Maybe PrecisionNum  
checkOrder o _ _  | o <= 0    = Nothing
                  | otherwise = Just (minPrecision o)  

checkDimension :: (Integral a) => Int -> Int -> [a] -> Maybe PrecisionNum  
checkDimension _ d _ | d <= 0    = Nothing
                     | otherwise = Just (minPrecision d)  

checkPoints :: (Integral a) =>  Int -> Int -> [a] -> Maybe [PrecisionNum]  
checkPoints o d p =  assert(fromIntegral d == length p)  
                     mapM (`mkPrecisionNum` o) p

toParameters :: (Ord a, Num a, Integral a) => Int -> Int -> [a] -> Maybe (PrecisionNum, PrecisionNum, [PrecisionNum]) 
toParameters order dimension points = do o <- checkOrder order dimension points 
                                         d <- checkDimension order dimension points 
                                         p <- checkPoints order dimension points  
                                         return (o, d, p) 


