{-# LANGUAGE RankNTypes #-} 
-- | The following implements 
-- /Compact Hilbert Indices - Technical Report -- CS-2006-07/ 
-- by Chris Hamilton.  At the time of writing this comment, is
-- found at:
-- <https://www.cs.dal.ca/sites/default/files/technical_reports/CS-2006-07.pdf> 
-- 

module Data.Algorithm.Hilbert (
-- * Functions 
   pointToIndex
 , pointToIndex' 
 , indexToPoint 
 , indexToPoint' 
 , grayCode
 , grayCodeInverse
 , newD
 , newE
)
 where  
import Data.Algorithm.Hilbert.Types 
import Data.Algorithm.Hilbert.Utility 
import Data.Bits
import Data.List (transpose) 
import Data.Maybe 

-- | pointToIndex takes an order, dimension, and point (represented by a list), 
-- and returns the corresponding Hilbert index.  Some examples follow which illustrate 
-- how to use the function.  
--
-- Imagine that you want to make a locality preserving ordering over the IPv4 address space, 
-- like that illustrated at <http://xkcd.com/195/> 
-- 
-- That diagram is a two dimensional Hilbert curve, tiled 16 x 16, more specifically, it has order equal to logBase 2 16 = 4
--
-- If we wished to construct such a digram, how would we calculate the octet that should occur
-- in the upper right corner of each square, given the co-ordinates of each square?
--
-- We can use the pointToIndex function. 
-- pointToIndex order dimension point 
-- For the top-left corner.   
--
-- >  pointToIndex 4 2 [0, 0] = 0  
--
-- For the bottom-right corner; 
--
-- >  pointToIndex 4 2 [15, 15] = 170 
--
-- For MIT, at co-ordinates x = 5, y = 1 relative to the top left corner.  
--
-- >  pointToIndex 4 2 [1 , 5]  
-- > = 18

pointToIndex :: (Integral u) =>         Int -- ^ The 'order' of the Hilbert curve.  
                                     -> Int -- ^ The dimension of the Hilbert curve. 
                                     -> [u] -- ^ A list specifying the a point in the Hilbert space
                                     -> Maybe u  -- ^ The resulting Hilbert index.  
pointToIndex order dimension point = do (o, d, p) <- toParameters order dimension point    
                                        c <- convertPointToHypercube p  
                                        -- Each element in c has precision 
                                        -- exactly equal to its dimension.  
                                        hi <- pointToIndex' 0 0 o d c
                                        return $ fromIntegral (value hi)  


pointToIndex' ::  PrecisionNum -> PrecisionNum -> PrecisionNum -> PrecisionNum -> [PrecisionNum] -> Maybe PrecisionNum
pointToIndex' _ _ _ _         []      = Just $ minPrecision (0::Integer)  
pointToIndex' e d order dimension (x:xs)  
                                = do let v1 = w `shiftL` shiftAmount 
                                     v2 <- pointToIndex' e' d' order dimension xs  
                                     return $ v1 .|. v2 
                                   where shiftAmount = length xs * fromIntegral dimension 
                                         w  = grayCodeInverse t 
                                         t  = transform e d x
                                         e' = newE e w d  
                                         d' = newD d w dimension 
     
-- | indexToPoint provides the inverse mapping for pointToIndex. 
-- 
-- Adopting the example from pointToIndex above, we can see that: 
-- 
-- > indexToPoint 4 2 18  
-- > = [1,5]
--
-- Another use for indexToPoint is to create a (roughly) continuous 
-- RGB palette from a one dimensional interval, for use in data
-- visualisation.  For this application, assuming RGB with an 8 bit color
-- depth on each component, we need:  
--  
--           - order      = logBase 2 256 = 8  
--
--           - dimension                 =  3 
-- 
-- We can now calculate the RGB color corresponding to any number in the
-- interval 0 .. (2^24)-1 
--
-- > indexToPoint 8 3 167
-- > = [1,7,7]
--
-- > indexToPoint 8 3 1000
-- > [10,0,4]

indexToPoint :: (Integral u, Num u) => 
                Int -- ^ The 'order' of the Hilbert curve.  
             -> Int -- ^ The dimension of the Hilbert curve. 
             -> u -- ^ An index in the Hilbert space
             -> Maybe [u]  -- ^ The resulting Hilbert point.  
indexToPoint order dimension i = do inverse <- sequence (indexToPoint' 0 0 (minPrecision order) (minPrecision dimension) c)  
                                    return $ map boolToInteger ( reverse $ transpose $ map (`integerToBool` dimension) $ reverse inverse) 
                                       where 
                                            -- convertInteger's second parameter is the number of bits in each chunk. 
                                            -- For example, if dimension is 2, 2 bits in each chunk.
                                            c =  fromJust $ convertInteger (minPrecision i) (fromIntegral dimension) (fromIntegral order)  

boolToInteger :: forall a. Num a => [Bool] -> a
boolToInteger (x:xs) | not x      = 0 + 2*boolToInteger xs
                     | otherwise  = 1 + 2*boolToInteger xs
boolToInteger [] = 0

integerToBool :: (Integral b, Bits a, Num a, Ord a) => a -> b -> [Bool]
integerToBool i bits =  map (testBit i)  [0.. fromIntegral bits-1]

indexToPoint' ::  PrecisionNum -> PrecisionNum -> PrecisionNum -> PrecisionNum -> [PrecisionNum] -> [Maybe PrecisionNum] 
indexToPoint' _ _ _  _         []      = [] 
indexToPoint' e d order dimension (w:ws)   = Just t : indexToPoint' e' d' order dimension ws  
                                           where l = grayCode w 
                                                 t = inverseTransform e d l  
                                                 e' = newE e w d  
                                                 d' = newD d w dimension 

-- | Helper function for calculation of pointToIndex and
-- indexToPoint.  See Hamilton, Algorithm 2 and Algorithm 3.  
-- FIXME: dimension is unused.  
newE :: PrecisionNum -> PrecisionNum -> PrecisionNum -> PrecisionNum 
newE e w d   =  e `xor` b 
                 where b = entryPoint w `rotateL` amount
                       amount = fromIntegral (d + 1) 


-- | Helper function for calculation of pointToIndex and
-- indexToPoint.  See Hamilton, Algorithm 2 and Algorithm 3.  
newD ::  PrecisionNum -> PrecisionNum  -> PrecisionNum -> PrecisionNum 
newD d w dimension = minPrecision vv `mod` dimension
                      where vv = value d + 1 + value (direction w dimension) 
                      -- FIXME: Perform as an integer operation, not fixed width. 

