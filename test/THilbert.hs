{-# LANGUAGE RankNTypes #-} 
module THilbert where
import Control.Monad (replicateM)  
import Control.Applicative (liftA, liftA3, (<$>)) 
import Data.Algorithm.Hilbert 
import Data.Algorithm.Hilbert.Types 
import Data.Algorithm.Hilbert.Utility 
import Data.Bits 
import Data.List (sort) 
import Data.Maybe
import Instances ()  
import Test.HUnit
import Test.QuickCheck

np :: Int -> Int -> PrecisionNum 
np a b = fromJust $ mkPrecisionNum a b 



-- Type / Instance / Utility

prop_boolNumBijectiveSmall :: Property
prop_boolNumBijectiveSmall  = forAll (choose(2, 1000)) $ \i -> 
                         property (i == j i) 
                            where j x = (value . fromJust . boolToNum . numToBool) (minPrecision x) :: Integer 
prop_boolNumBijectiveLarge :: Property
prop_boolNumBijectiveLarge  = forAll (choose(2^(128::Integer), 2^(128::Integer) - 1000)) $ \i -> 
                         property (i == j i) 
                            where j x = (value . fromJust . boolToNum . numToBool) (minPrecision x) :: Integer 

test_NumToBool :: Assertion 
test_NumToBool = [True, False, True, True] @=? (numToBool) (fromJust (mkPrecisionNum (13::Integer) (4::Integer))) 

test_BoolToNum :: Assertion 
test_BoolToNum = let
                   expected = mkPrecisionNum (13::Int) (4::Int) 
                 in 
                   expected @=? (boolToNum [True, False, True, True]) 


test_ConvertPointToHypercubeA :: Assertion  
test_ConvertPointToHypercubeA = let point = fromJust $ sequence [mkPrecisionNum (5::Integer) (3::Integer), mkPrecisionNum (6::Integer) (3::Integer)]
                                    hyper =  sequence [mkPrecisionNum (3::Integer) (2::Integer)
                                         , mkPrecisionNum (1::Integer) (2::Integer)
                                         , mkPrecisionNum (2::Integer) (2::Integer)] 
                                 in
                                        hyper @=? (convertPointToHypercube point)  


test_ConvertPointToHypercubeB :: Assertion  
test_ConvertPointToHypercubeB = let point = sequence [mkPrecisionNum (1::Integer) (3::Integer), mkPrecisionNum (2::Integer) (3::Integer), mkPrecisionNum (3::Integer) (3::Integer)] 
                                    hyper =  sequence [mkPrecisionNum (0::Integer) (3::Integer), mkPrecisionNum (3::Integer) (3::Integer), mkPrecisionNum (5::Integer) (3::Integer)] 
                                in
                                    (point >>= convertPointToHypercube) @=? hyper  


test_ConvertIntegerA :: Assertion  
test_ConvertIntegerA = let input = mkPrecisionNum (45::Integer) (6::Integer)
                           expected = (sequence [mkPrecisionNum (2::Integer) (2::Integer), mkPrecisionNum (3::Integer) (2::Integer), mkPrecisionNum (1::Integer) (2::Integer)]) :: Maybe [PrecisionNum]  
                     in 
                         (input >>= \i -> convertInteger i (2::Int) (3::Int)) @=? expected  

test_ConvertIntegerB :: Assertion 
test_ConvertIntegerB = let input = mkPrecisionNum (5::Integer) (3::Integer)
                           expected = (sequence [mkPrecisionNum (5::Integer) (3::Integer)])  
                      in 
                          (input >>= \i -> convertInteger i 3 1) @=? expected


-- When we divide an integer into pieces, the precisions 
-- are all equal since our aim was to divide into equal size pieces. 
prop_ConvertIntegerEqualPieces :: PrecisionNum -> Int -> Int -> Property 
prop_ConvertIntegerEqualPieces testNumber chunks chunkSize = value testNumber > 2 &&
                                                 precision testNumber <= chunks * chunkSize &&   
                                                 chunks >= 1 && chunks < 10000 &&  
                                                 chunkSize >= 1 && chunkSize < 100 
                               ==>
                               -- Pick a size that is slightly too large.  
                               let converted = convertInteger testNumber chunkSize chunks  
                                   precisions = (map (fromIntegral . precision)) <$> converted 
                               in 
                                   Just (replicate chunks (chunkSize)) == precisions  

---- See section 2.1.3 
prop_rotationsBijective ::  Property
prop_rotationsBijective         = forAll (choose (10, 1000::Integer)) $ \val -> 
                                  forAll (choose (5,    20::Int    )) $ \amt -> 
                                  let v1 = minPrecision val 
                                   in 
                                      v1 == rotateL (rotateR v1 amt) amt

test_RotateLmodnA :: Assertion  
test_RotateLmodnA =  (mkPrecisionNum (3::Integer) (4::Integer)) @=?
                         do v <- mkPrecisionNum (9::Integer) (4::Integer) 
                            return $ rotateL v 1  

test_RotateLmodnB :: Assertion  
test_RotateLmodnB =  (mkPrecisionNum (0::Integer) (4::Integer)) @=?
                         do v <- mkPrecisionNum (0::Integer) (4::Integer) 
                            return $ rotateL v 0  

test_RotateLmodnC :: Assertion  
test_RotateLmodnC =  (mkPrecisionNum (0::Integer) (4::Integer)) @=?
                         do v <- mkPrecisionNum (0::Integer) (4::Integer) 
                            return $ rotateL v 1  

test_RotateLmodnD :: Assertion  
test_RotateLmodnD =  (mkPrecisionNum (1::Integer) (4::Integer)) @=?
                         do v <- mkPrecisionNum (1::Integer) (4::Integer) 
                            return $ rotateL v 0  


test_RotateRmodnA :: Assertion  
test_RotateRmodnA = (mkPrecisionNum (5::Integer) (32::Integer)) @=? 
                         do v <- mkPrecisionNum (10::Integer) (32::Integer) 
                            return $ rotateR v 1 

test_RotateRmodnB :: Assertion  
test_RotateRmodnB = (mkPrecisionNum (12::Integer) (4::Integer)) @=?  
                         do v <- mkPrecisionNum (9::Integer) (4::Integer) 
                            return $ rotateR v 1 




prop_precisionRepresents :: Int -> Property 
prop_precisionRepresents val = val > 1 ==> 
                            ((precisionRequired (fromIntegral val)) :: Int) 
                         == ceiling ((logBase 2 (fromIntegral val + 1)) :: Double)  

test_PrecisionRequiredA :: Assertion 
test_PrecisionRequiredA = 3 @=? precisionRequired 4  


test_PrecisionRequiredB :: Assertion 
test_PrecisionRequiredB = 1 @=? precisionRequired 1  


test_PrecisionRequiredC :: Assertion 
test_PrecisionRequiredC = 1 @=? precisionRequired 0  

test_MkPrecisionNum :: Assertion 
test_MkPrecisionNum = value (fromJust (mkPrecisionNum (18014398509481974::Integer) (54::Integer))) @=? (18014398509481974::Integer) 


test_Num :: Assertion 
test_Num = PrecisionNum {value = 2^(99 :: Integer), precision=100} @=?  (2^(99::Integer) :: PrecisionNum)  

test_ShiftRALarge :: Assertion
test_ShiftRALarge = PrecisionNum {value = 2^(99 :: Integer), 
                            precision=100} @=? shiftRA (PrecisionNum {value = 2^(100::Integer), precision = 102}) (1::Int) 

test_ShiftRASmall :: Assertion 
test_ShiftRASmall =  (np 3 3) @=?  shiftRA (np 6 4) (1::Int)   

test_ShiftRAPrecision :: Assertion 
test_ShiftRAPrecision =  3 @=? precision ( (np 6 4) `shiftRA` (1::Int))   


test_ShiftLASmall :: Assertion 
test_ShiftLASmall =  (np 6 4) @=?  shiftLA (np 3 3) (1::Int)   


test_ShiftLAPrecision :: Assertion 
test_ShiftLAPrecision =  4 @=? precision ( (np 3 3) `shiftLA` (1::Int))   


---- <--------------------------------> -- 
---- Utility Functions --  

---- Lemma 2.3 and Algorithm 1 
---- The bit that changes in progressing from gray code m to its successor
---- gray code n is determined by trailingSetBits m. 
prop_SuccessorBitByTrailingBits :: PrecisionNum -> Property
prop_SuccessorBitByTrailingBits i = (i >= 0) 
                                    ==> testBit  (oneSetBit i)  (fromIntegral $ trailingSetBits i)  -- The bit is set, and 
                                    &&  clearBit (oneSetBit i)  (fromIntegral $ trailingSetBits i)  == 0  
                                      where oneSetBit x = (grayCode x) `xor` (grayCode (floatingPlus x 1))  

---- Corollary 2.5  
---- g(i) is symmetric
---- Since trailingSetBits needs either zero or a positive integer, 
---- we have the conditions i <= 2^n - 2.  This amounts to the condition 
---- expressed above,       i <  2^n - 1.  
prop_trailingSetBitsSymmetric :: Property
prop_trailingSetBitsSymmetric = forAll (choose (10, 1000::Integer)) $ \i -> 
                                forAll (suchThat (choose (5, 20::Integer)) (\nn -> 0 < 2^nn - 1 - i)) $ \n ->
                                let 
                                     v1 = minPrecision i 
                                     v2 = minPrecision (2^n - 2 - i)  
                                in 
                                     trailingSetBits v1 == trailingSetBits v2 

---- Lemma 2.6 
prop_entryPointExitPointSymmetric ::  Property
prop_entryPointExitPointSymmetric   = forAll (choose (10, 1000::Integer)) $ \i -> 
                                      forAll (suchThat (choose (5, 20::Integer)) (\nn -> 0 < 2^nn - 1 - i)) $ \n ->
                                      let   v1 = minPrecision $ (2^n) - 1 - i 
                                            v2 = minPrecision $ (2^(n-1)::Integer)  
                                            v3 = minPrecision i 
                                            v4 = minPrecision n  
                                      in 
                                            entryPoint v3 == ((exitPoint v1 v4) `xor` v2) 
 

---- Corollary 2.7 
---- direction is symmetric
prop_directionSymmetric :: Property
prop_directionSymmetric  = forAll (choose (10, 1000::Integer)) $ \i -> 
                           forAll (suchThat (choose (5, 20::Integer)) (\nn -> 0 < 2^nn - 1 - i)) $ \n ->
                           let   v1 = minPrecision i 
                                 v2 = minPrecision n 
                                 v3 = minPrecision $ 2^n - 1 - i 
                           in 
                                 direction v1 v2 == direction v3 v2  
                                 
--
--
---- Further part of Corollary 2.7  
----n = 2 
----   e(i) d(i)  f(i)   e(i) `xor` 2^d(i)  
----0  [00] 0     [01]                 01  
----1  [00] 1     [10]                 10  
----2  [00] 1     [10]                 10  
----3  [11] 0     [10]                 10  
prop_entryDirectionExit :: Property 
prop_entryDirectionExit = forAll (choose (10, 1000::Integer)) $ \i -> 
                          forAll (suchThat (choose (5, 20::Integer)) (\nn -> 0 < 2^nn - 1 - i)) $ \n ->
                             let   v1 = minPrecision i 
                                   v2 = minPrecision n 
                             in 
                               ((entryPoint v1) `xor` (floatingTwoExp(direction v1 v2))) == exitPoint v1 v2
--
--
---- This property is described at the top of page 13 of Hamilton. 
prop_directionWhenIZero ::  Property 
prop_directionWhenIZero = forAll (choose (10, 100::Integer)) $ \n -> 
                          let v1 = minPrecision n  
                           in 
                          direction 0 v1 == 0 

---- This property is described at the top of page 13 of Hamilton. 
---- Note the stricter condition that i < 2^n - 1 
prop_directionOneHigherDimension ::  Property 
prop_directionOneHigherDimension   = forAll (choose (10, 1000::Integer)) $ \i -> 
                                     forAll (suchThat (choose (5, 20::Integer)) (\nn -> 0 < 2^nn - 1 - i)) $ \n ->
                                     let v1 = minPrecision i 
                                         v2 = minPrecision n 
                                         v3 = minPrecision (n+1)  
                                     in 
                                         direction v1 v2 == direction v1 v3 


 
prop_transformBijective ::  Property
prop_transformBijective                 =  forAll (choose(2::Int, 10)) $ \dimension -> 
                                           forAll (suchThat (choose(1::Int, 1000000)) (\b' -> b' < 2^dimension)) $ \b -> 
                                           forAll (suchThat (choose(1::Int, 1000000)) (\e' -> e' < 2^dimension)) $ \e -> 
                                           forAll (suchThat (choose(1::Int, 1000000)) (\d' -> d' < 2^dimension)) $ \d -> 
                                         let bPrecision = fromJust $ mkPrecisionNum b dimension
                                             ePrecision = fromJust $ mkPrecisionNum e dimension  
                                             dPrecision = fromJust $ mkPrecisionNum d dimension 
                                           in  
                                           property $ bPrecision == (inverseTransform ePrecision dPrecision (transform ePrecision dPrecision bPrecision))


test_EntryPointA :: Assertion  
test_EntryPointA = fromJust $ do v2 <- mkPrecisionNum (0::Integer) (4::Integer)  
                                 v1 <- mkPrecisionNum (0::Integer) (4::Integer) 
                                 return $ v2 @=? entryPoint v1 

test_EntryPointB :: Assertion 
test_EntryPointB = fromJust $ do v2 <- mkPrecisionNum (0::Integer) (4::Integer)  
                                 v1 <- mkPrecisionNum (1::Integer) (4::Integer)  
                                 return $ v2 @=? entryPoint v1

test_EntryPointC :: Assertion 
test_EntryPointC = fromJust $ do v2 <- mkPrecisionNum (0::Integer) (4::Integer) 
                                 v1 <- mkPrecisionNum (2::Integer) (4::Integer) 
                                 return $ v2 @=? entryPoint v1 

test_EntryPointD :: Assertion 
test_EntryPointD = fromJust $ do v2 <- mkPrecisionNum (3::Integer) (4::Integer) 
                                 v1 <- mkPrecisionNum (3::Integer) (4::Integer)  
                                 return $ v2 @=? entryPoint v1

test_ExitPointA :: Assertion  
test_ExitPointA = fromJust $ do v2 <- mkPrecisionNum (1::Integer) (2::Integer) 
                                v1 <- mkPrecisionNum (0::Integer) (2::Integer) 
                                return $ v2 @=? (exitPoint v1 2) 


test_ExitPointB :: Assertion 
test_ExitPointB = fromJust $ do v2 <- mkPrecisionNum (2::Integer) (2::Integer) 
                                v1 <- mkPrecisionNum (1::Integer) (2::Integer) 
                                return $ v2 @=? exitPoint v1 2


test_ExitPointC :: Assertion 
test_ExitPointC = (mkPrecisionNum (2::Integer) (2::Integer)) @=? 
                 ((mkPrecisionNum (2::Integer) (2::Integer)) >>= (\x -> return $ exitPoint x 2)) 

test_ExitPointD :: Assertion 
test_ExitPointD = (mkPrecisionNum (2::Integer) (2::Integer)) @=? 
                 ((mkPrecisionNum (3::Integer) (2::Integer)) >>= (\x -> return $ exitPoint x 2)) 

 
test_TransformA :: Assertion  
test_TransformA =  let e = mkPrecisionNum (0::Integer) (2::Integer)  
                       d = mkPrecisionNum (0::Integer) (2::Integer) 
                       b = mkPrecisionNum (1::Integer) (2::Integer) -- T_(e,d) == 1 
                       res = mkPrecisionNum (2::Integer) (2::Integer)  
                   in 
                       res @=? liftA3 transform e d b

-- Examples from page 18. 
test_TransformB :: Assertion
test_TransformB = let e = mkPrecisionNum (0::Integer) (2::Integer) 
                      d = mkPrecisionNum (1::Integer) (2::Integer)
                      b = mkPrecisionNum (3::Integer) (2::Integer) 
                      res = mkPrecisionNum (3::Integer) (2::Integer)  
                  in 
                      res @=? liftA3 transform e d b

test_TransformC :: Assertion
test_TransformC = let e = mkPrecisionNum (0::Integer) (2::Integer)  
                      d = mkPrecisionNum (1::Integer) (2::Integer)
                      b = mkPrecisionNum (2::Integer) (2::Integer) 
                      res = mkPrecisionNum (2::Integer) (2::Integer)
                  in 
                      res @=? liftA3 transform e d b 

test_TransformD :: Assertion
test_TransformD = let e = mkPrecisionNum (3::Integer) (2::Integer) 
                      d = mkPrecisionNum (0::Integer) (2::Integer) 
                      b = mkPrecisionNum (1::Integer) (2::Integer)
                      res = mkPrecisionNum (1::Integer) (2::Integer)
                  in 
                      res @=? liftA3 transform e d b
               
test_InverseTransformA :: Assertion  
test_InverseTransformA =  let e = mkPrecisionNum (1::Integer) (4::Integer)
                              d = mkPrecisionNum (10::Integer) (4::Integer)
                              b = mkPrecisionNum (0::Integer) (4::Integer)
                              res = mkPrecisionNum (1::Integer) (4::Integer)
                          in 
                             res @=? liftA3 inverseTransform e d b


test_InverseTransformB :: Assertion  
test_InverseTransformB =  let e = mkPrecisionNum (1::Integer) (4::Integer)
                              d = mkPrecisionNum (10::Integer) (4::Integer)
                              b = mkPrecisionNum (1::Integer) (4::Integer)
                              res = mkPrecisionNum (9::Integer) (4::Integer)
                          in 
                              res @=? liftA3 inverseTransform e d b

test_InverseTransformC :: Assertion  
test_InverseTransformC =  let e = mkPrecisionNum (0::Integer) (2::Integer)
                              d = mkPrecisionNum (0::Integer) (2::Integer)
                              b = mkPrecisionNum (2::Integer) (2::Integer)
                              res = mkPrecisionNum (1::Integer) (2::Integer)
                          in 
                              res @=? liftA3 inverseTransform e d b

test_InverseTransformD :: Assertion  
test_InverseTransformD =  let e = mkPrecisionNum (3::Integer) (2::Integer)
                              d = mkPrecisionNum (1::Integer) (2::Integer)
                              b = mkPrecisionNum (0::Integer) (2::Integer)
                              res = mkPrecisionNum (3::Integer) (2::Integer)  
                          in 
                             res @=? liftA3 inverseTransform e d b

test_InverseTransformE :: Assertion  
test_InverseTransformE =  let e = mkPrecisionNum (3::Integer) (2::Integer)
                              d = mkPrecisionNum (0::Integer) (2::Integer)
                              b = mkPrecisionNum (0::Integer) (2::Integer)
                              res = mkPrecisionNum (3::Integer) (2::Integer)  
                          in 
                             res @=? liftA3 inverseTransform e d b

test_Exp :: Assertion 
test_Exp   = let a = fromJust $ mkPrecisionNum (3::Integer) (3::Integer) 
            in 
                2^a @=? (8::Integer)   

test_DirectionA :: Assertion  
test_DirectionA = fromJust $  do v <- mkPrecisionNum (1::Integer) (2::Integer) 
                                 p <- mkPrecisionNum (2::Integer) (2::Integer) 
                                 r <- mkPrecisionNum (1::Integer) (2::Integer) 
                                 return $ r @=? (direction v p)   


test_DirectionB :: Assertion  
test_DirectionB = fromJust $ do v <- mkPrecisionNum (0::Integer) (3::Integer) 
                                p <- mkPrecisionNum (3::Integer) (3::Integer)  
                                r <- mkPrecisionNum (0::Integer) (3::Integer) 
                                return $ r @=? (direction v p)  



-- <----------------------------> --- 




-- Graycode tests --- 
---- Transformation is bijective. 
prop_graycodeBijective :: PrecisionNum -> Property 
prop_graycodeBijective i = (value i >= 0) 
                           ==> grayCodeInverse (grayCode i) == i 


test_GrayCodeA :: Assertion 
test_GrayCodeA = 8943502960915236902818643137753611301881986722824192 @=? grayCode 10000000000000000000000000000000000000000000000000000  

test_GrayCodeB :: Assertion 
test_GrayCodeB = let i = PrecisionNum {value = 10000000000000000000000000000000000000000000000000000, precision = 173} 
                     byInstance = i `xor` shifted where shifted = i `shiftR` 1 
                     fixedVersion = grayCode i 
                 in 
                     byInstance @=? fixedVersion 

test_GrayCodeC :: Assertion 
test_GrayCodeC = 27670116110564327424 @=? grayCode ( 2^(64::Integer) )  

test_GrayCodeD :: Assertion 
test_GrayCodeD = 55340232221128654848 @=? grayCode ( 2^(65::Integer) )  




---- Lemma 2.4 
---- Gray code is symmetric
prop_grayCodeSymmetric :: Property
prop_grayCodeSymmetric = forAll (choose (10, 1000::Integer)) $ \i -> 
                         forAll (suchThat (choose (5, 20::Integer)) (\nn -> 0 < 2^nn - 1 - i)) $ \n ->
                         let   v1 = minPrecision ((2^n) - 1 - i)  
                               v2 = minPrecision (2^(n-1)::Integer)  
                               v3 = minPrecision i  
                         in 
                         grayCode v1  == (grayCode v3) `xor` v2   


prop_grayCodeSymmetricLarge :: Property
prop_grayCodeSymmetricLarge = forAll (choose (2^(100::Integer)::Integer, 2^(100::Integer)+10000)) $ \i -> 
                         forAll (suchThat (choose (105, 107::Integer)) (\nn -> 0 < 2^nn - 1 - i)) $ \n ->
                         let   v1 = minPrecision ((2^n) - 1 - i)  
                               v2 = minPrecision (2^(n-1)::Integer)  
                               v3 = minPrecision i  
                         in 
                         grayCode v1  == (grayCode v3) `xor` v2   



--- Argument to graycode, and its result have the same number of bits. 
prop_grayCodeMaintainsPrecision :: Integer -> Property
prop_grayCodeMaintainsPrecision n = n > 0 ==> 
                                    let v = minPrecision n 
                                       in 
                                    precision v == precision (grayCode v)  
                         

test_iGrayCode1 :: Assertion  
test_iGrayCode1 = let expected = mkPrecisionNum (2::Integer) (4::Integer)  
                      input    = mkPrecisionNum (3::Int) (4::Int) 
                  in 
                     liftA grayCodeInverse input @=? expected 

test_iGrayCode2 :: Assertion  
test_iGrayCode2 = let expected = mkPrecisionNum (1::Integer) (4::Integer)  
                      input = mkPrecisionNum (1::Integer) (4::Integer) 
                  in 
                     liftA grayCodeInverse input @=? expected 


test_iGrayCode3 :: Assertion  
test_iGrayCode3 = let expected = mkPrecisionNum (3::Integer) (40::Integer) 
                      input =    mkPrecisionNum (2::Integer) (40::Integer) 
                  in 
                     liftA grayCodeInverse input @=? expected 


--- Hilbert Tests. 

prop_hilbertBijectiveO32D32 :: Int ->  Property 
prop_hilbertBijectiveO32D32 i = i > 0 ==>  
                               let order = 32 
                                   dimension = 32 
                               in 
                                   property $  i  == fromJust (pointToIndex order dimension $ fromJust (indexToPoint order dimension (i)))   
 
prop_hilbertBijective :: Property
prop_hilbertBijective  =   forAll (choose (1, 6::Int)) $ \order -> 
                           forAll (choose (1, 6::Int)) $ \dimension -> 
                           forAll (suchThat (choose (1, 10000::Int)) (\v -> v < 2^(order*dimension))) $ \va -> 
                           property $  va == fromJust (pointToIndex order dimension $ fromJust (indexToPoint order dimension va))  
                            

prop_hilbertExhaustive :: Property 
prop_hilbertExhaustive = forAll (choose(1::Int, 4)) $ \order -> 
                         forAll (choose(1::Int, 3)) $ \dimension -> 
                         property $ -- Generate every possible point.  
                              let indexes = fromJust $ mapM (pointToIndex order dimension) (replicateM dimension [0..(2^order)-1]) 
                                in 
                              sort indexes == checkRange (order*dimension)  
                                  where  checkRange p =  [0 .. (2^p-1) ] :: [Int]  

test_HilbertExhaustive :: Assertion 
test_HilbertExhaustive = let order = 2
                             dimension = 3 
                             indexes = fromJust $ mapM (pointToIndex order dimension) (replicateM dimension [0..(2^order)-1]) 
                         in 
                             sort indexes @=? checkRange (order*dimension)   
                                  where  checkRange p =  [0 .. (2^p-1) ] :: [Int]  



test_SetTrailingBits :: Assertion  
test_SetTrailingBits = fromJust $ do v1 <- mkPrecisionNum (7::Integer) (20::Integer) 
                                     v2 <- mkPrecisionNum (3::Integer) (20::Integer) 
                                     return $ v2 @=? trailingSetBits v1

test_SetTrailingBits2 :: Assertion  
test_SetTrailingBits2 =  fromJust $ do v2 <- mkPrecisionNum (4::Integer) (50::Integer)  
                                       v1 <- mkPrecisionNum (15::Int) (50::Int) 
                                       return $ v2 @=? trailingSetBits v1  

test_SetTrailingBits3 :: Assertion  
test_SetTrailingBits3 = fromJust $ do v2 <- mkPrecisionNum (4::Int) (100::Int) 
                                      v1 <- mkPrecisionNum (15+128::Int) (100::Int) 
                                      return $ v2 @=? trailingSetBits v1 


test_SetTrailingBits4 :: Assertion  
test_SetTrailingBits4 = fromJust $ do v2 <- mkPrecisionNum (0::Integer) (2::Integer) 
                                      v1 <- mkPrecisionNum (2::Integer) (2::Integer) 
                                      return $ v2 @=? trailingSetBits v1  

test_SetTrailingBits5 :: Assertion  
test_SetTrailingBits5 = fromJust $ do v2 <- mkPrecisionNum (0::Integer) (20::Integer)  
                                      v1 <- mkPrecisionNum (128::Integer) (20::Integer)  
                                      return $ v2 @=? trailingSetBits v1  

-- Line where i = 2 
test_TransformRef1 :: Assertion  
test_TransformRef1 =  let e = mkPrecisionNum (0::Integer) (2::Integer) 
                          d = mkPrecisionNum (1::Integer) (2::Integer) 
                          b = mkPrecisionNum (3::Integer) (2::Integer)  -- l == (3::Integer) 
                          res = mkPrecisionNum (3::Integer) (2::Integer) -- T_(e, d) == (3::Integer)  
                      in 
                          res @=? liftA3 transform e d b 


-- Line where i = 1 
test_TransformRef2 :: Assertion  
test_TransformRef2 =  let e = mkPrecisionNum (0::Integer) (2::Integer)  
                          d = mkPrecisionNum (1::Integer) (2::Integer) 
                          b = mkPrecisionNum (2::Integer) (2::Integer)  -- l == (2::Integer) 
                          res = mkPrecisionNum (2::Integer) (2::Integer) -- T_(e,d) = (2::Integer) 
                      in 
                          res @=? liftA3 transform e d b 


-- Line where i = 0 
test_TransformRef3 :: Assertion  
test_TransformRef3 =  let e = mkPrecisionNum (3::Integer) (2::Integer)  
                          d = mkPrecisionNum (0::Integer) (2::Integer) 
                          b = mkPrecisionNum (1::Integer) (2::Integer) -- l == 2
                          res = mkPrecisionNum (1::Integer) (2::Integer) -- T_(e,d) = 1
                      in 
                          res @=? liftA3 transform e d b



-- Line where i = 2 
test_WRef1 :: Assertion 
test_WRef1 = let input  = mkPrecisionNum (3::Integer) (2::Integer)
                 out = mkPrecisionNum (2::Integer) (2::Integer) 
             in 
                 out @=? liftA grayCodeInverse input  

-- Line where i = 1 
test_WRef2 :: Assertion 
test_WRef2 = let input  = mkPrecisionNum (2::Integer) (2::Integer) 
                 out = mkPrecisionNum (3::Integer) (2::Integer)  
             in 
                 out @=? liftA grayCodeInverse input  

-- Line where i = 0   
test_WRef3 :: Assertion 
test_WRef3 = let input = mkPrecisionNum (1::Integer) (2::Integer) 
                 out = mkPrecisionNum (1::Integer) (2::Integer) 
             in 
                 out @=? liftA grayCodeInverse input 

-- Line where i = 2
test_ERef1 :: Assertion 
test_ERef1 = let e = mkPrecisionNum (0::Integer) (2::Integer)  
                 w = mkPrecisionNum (2::Integer) (2::Integer)
                 d = mkPrecisionNum (1::Integer) (2::Integer)  -- T_(e,d) == 3 
                 res = mkPrecisionNum (0::Integer) (2::Integer)
             in  
                 liftA3 newE e w d  @=? res  



-- Line where i = 1 
test_ERef2 :: Assertion 
test_ERef2 = let e = mkPrecisionNum (0::Integer) (2::Integer) 
                 w = mkPrecisionNum (3::Integer) (2::Integer)
                 d = mkPrecisionNum (1::Integer) (2::Integer)  -- T_(e,d) == 3 
                 res = mkPrecisionNum (3::Integer) (2::Integer)
             in  
                 liftA3 newE e w d  @=? res  



-- Line where i = 0 
test_ERef3 :: Assertion 
test_ERef3 = let e = mkPrecisionNum (3::Integer) (2::Integer)
                 w = mkPrecisionNum (1::Integer) (2::Integer)
                 d = mkPrecisionNum (0::Integer) (2::Integer)
                 expected = mkPrecisionNum (3::Integer) (2::Integer)
             in  
                 expected @=? liftA3 newE e w d 

-- Line where i =2  
test_DRef1 :: Assertion 
test_DRef1 = let w = mkPrecisionNum (2::Integer) (2::Integer)  
                 d = mkPrecisionNum (1::Integer) (2::Integer)  -- T_(e,d) == 3 
                 dimension = mkPrecisionNum (2::Integer) (2::Integer)
                 res = mkPrecisionNum (1::Integer) (2::Integer)
             in  
                 liftA3 newD d w dimension @=? res  

-- Line where i =1 
test_DRef2 :: Assertion 
test_DRef2 = let w = mkPrecisionNum (3::Integer) (2::Integer) 
                 d = mkPrecisionNum (1::Integer) (2::Integer) -- T_(e,d) == 3 
                 dimension = mkPrecisionNum (2::Integer) (2::Integer)
                 res = mkPrecisionNum (0::Integer) (2::Integer)
             in  
                 liftA3 newD d w dimension @=? res  

-- Line where i =0 
test_DRef3 :: Assertion 
test_DRef3 = let w = mkPrecisionNum (1::Integer) (2::Integer)   
                 d = mkPrecisionNum (0::Integer) (2::Integer)  -- T_(e,d) == 3 
                 dimension = mkPrecisionNum (2::Integer) (2::Integer)
                 res = mkPrecisionNum (0::Integer) (2::Integer) 
             in  
                 liftA3 newD d w dimension @=? res  



-- The following is a detailed breakdown for pointToIndex 3 2 [5, 6])
-- There are three stages in the calculation. 

test_HilbertIndex :: Assertion  
test_HilbertIndex = (mkPrecisionNum (45::Integer) (6::Integer)) @=? (pointToIndex 3 2 [5, 6]) 


-- pointToIndex' 0 0 3 2 [3,1,2] 
test_Stage1overall :: Assertion
test_Stage1overall = Just (np 45 6) @=? 
                    pointToIndex' (np 0 2) (np 0 2) (np 3 6) (np 2 6) [np 3 2,np 1 2,np 2 2]  

test_Stage1overallPrecision :: Assertion
test_Stage1overallPrecision =  6 @=? 
                    precision (fromJust (pointToIndex' (np 0 2) (np 0 2) (np 3 6) (np 2 6) [np 3 2, np 1 2, np 2 2] ) )   

test_Stage1ShiftL :: Assertion
test_Stage1ShiftL = 32 @=? (np 2 2) `shiftL` 4  
--  t = 3 = transform 0 0 3 2  

test_Stage1t :: Assertion
test_Stage1t = (np 3 2) @=? transform (np 0 2) (np 0 2) (np 3 2)

test_Stage1tPrecision :: Assertion
test_Stage1tPrecision =  2 @=?  precision ( transform (np 0 2) (np 0 2) (np 3 2)) 
--  w = 2 = grayCodeInverse 3

test_Stage1w :: Assertion
test_Stage1w = (np 2 2) @=? grayCodeInverse (np 3 2)  

test_Stage1wPrecision :: Assertion
test_Stage1wPrecision = 2 @=? precision (grayCodeInverse (np 3 2))  

--  e' = 0 = newE 0 2 0  
test_Stage1e' :: Assertion
test_Stage1e' = (np 0 2) @=? newE (np 0 2) (np 2 2) (np 0 2)  

test_Stage1e'Precision :: Assertion
test_Stage1e'Precision =  2 @=? precision (newE (np 0 2) (np 2 2) (np 0 2) )  

--  d' = 0 = newD 0 2 2
test_Stage1d' :: Assertion
test_Stage1d' = (np 0 2) @=? newD (np 0 2) (np 2 2) (np 2 2)  

test_Stage1d'Precision :: Assertion
test_Stage1d'Precision = 2 @=? precision (newD (np 0 2) (np 2 2) (np 2 2))  
-- (2 << (2 * 2)) .|. 

-- pointToIndex' 0 0 3 2 [1, 2] 
test_Stage2overall :: Assertion
test_Stage2overall = Just (np 13 4) @=? pointToIndex' (np 0 2) (np 0 2) (np 3 6) (np 2 6) [np 1 2, np 2 2] 
-- t = 2 = transform 0 0 1 2 

test_Stage2t :: Assertion
test_Stage2t  = (np 2 2) @=? transform (np 0 2) (np 0 2) (np 1 2) 

-- w = 3 = grayCodeInverse 2
test_Stage2w :: Assertion
test_Stage2w  = (np 3 2) @=? grayCodeInverse (np 2 2)  
-- e' = 3 = newE 0 3 0 

test_Stage2e' :: Assertion
test_Stage2e' = (np 3 2) @=? newE (np 0 2) (np 3 2) (np 0 2) 
-- d' = 1 = newD 0 3 2

test_Stage2d' :: Assertion
test_Stage2d' = (np 1 2) @=? newD (np 0 2) (np 3 2) (np 2 2)  
-- (3 << (1 * 2)) .|. 
 
 
-- pointToIndex' 3 1 3 2 [2]
test_Stage3overall :: Assertion
test_Stage3overall = Just (np 1 6) @=? pointToIndex' (np 3 2) (np 1 2) (np 3 6) (np 2 6) [np 2 2]

-- t = 1 = transform 3 1 2 2
test_Stage3transform :: Assertion
test_Stage3transform = (np 1 2) @=? transform (np 3 2) (np 1 2) (np 2 2) 

test_Stage3transformDim :: Assertion
test_Stage3transformDim = 2 @=? precision (transform (np 3 2) (np 1 2) (np 2 2)) 

test_Stage3transformXor :: Assertion
test_Stage3transformXor = res @=? (b `xor` e) where
                              b = np 2 6 
                              e = np 3 6 
                              res = np 1 6

test_Stage3transformRotateR :: Assertion
test_Stage3transformRotateR = res @=? (z `rotateR` amount) 
                              where
                                 z = np 1 3  
                                 amount = fromIntegral (d+1) 
                                 d = np 1 3  
                                 res = fromJust $ mkPrecisionNum (2::Integer) (3::Integer)  

-- w = 1 = grayCodeInverse 1

test_Stage3w :: Assertion
test_Stage3w = (np 1 2) @=? grayCodeInverse (np 1 2) 
-- e' = 3 = newE 3 1 1 
test_Stage3e' :: Assertion
test_Stage3e' = (np 3 2) @=? newE (np 3 2) (np 1 2) (np 1 2) 
-- d' = 1 = newD 1 1 1 
test_Stage3d' :: Assertion
test_Stage3d' = (np 1 2) @=? newD (np 1 2) (np 1 2) (np 2 2)  
-- 1 << (0 * 2) 
 
-- = 32 .|. 12 .|. 1 = 45  


test_HilbertIndex2 :: Assertion  
test_HilbertIndex2 = (mkPrecisionNum (2::Integer) (2::Integer))  @=? (pointToIndex 2 2 [1, 1])

test_HilbertIndex3 :: Assertion  
test_HilbertIndex3 = (mkPrecisionNum (0::Integer) (5::Integer)) @=? (pointToIndex 10 10 [0,0,0,0,0,0,0,0,0,0]) 
test_HilbertIndex4 :: Assertion  
test_HilbertIndex4 = (mkPrecisionNum (48::Integer) (6::Integer)) @=? (pointToIndex 3 2 [3, 7]) 

--- Hilbert index inverse of 48, order 3, dimension 2 ----------------
test_HilbertIndexInverse :: Assertion  
test_HilbertIndexInverse = ([3, 7]::[Integer]) @=? (fromJust $ indexToPoint (3) (2) (48)) 
-- Order 3, Dimension 2, Index 48 
-- ->>> [1,3,3] == indexToPoint' 0 0 3 2 [3, 0, 0] 
test_HilbertIndexInverse' :: Assertion  
test_HilbertIndexInverse' = ([minPrecision (1 :: Int), minPrecision (3 :: Int), minPrecision (3 :: Int)]) @=? 
                     (fromJust $ sequence (indexToPoint' (minPrecision (0::Int)) (minPrecision (0::Int)) (minPrecision (3::Int)) (minPrecision (2::Int)) [minPrecision (3::Int),minPrecision (0::Int), minPrecision (0::Int) ]))


-- Components of indexToPoint' 0 0 3 2 [3, 0, 0] 
-- w = 3 
-- grayCode 3 = 2
-- e = 0 
-- d = 0 
-- order = 3 
-- dimension = 2 
-- grayCode w 
test_HilbertIndexInverseGrayCode :: Assertion
test_HilbertIndexInverseGrayCode = 2 @=? (grayCode 3) 
-- l = grayCode w 
-- inverseTransform e d l dimension
test_HilbertIndexInverseInverseTransform :: Assertion
test_HilbertIndexInverseInverseTransform = 1 @=? (inverseTransform 0 0 2 ) 
-- newE e w d 
test_HilbertIndexInverseNewE :: Assertion
test_HilbertIndexInverseNewE = 3 @=? (newE 0 3 0) 
-- newD d w dimension 
test_HilbertIndexInverseNewD :: Assertion
test_HilbertIndexInverseNewD = 1 @=? (newD 0 3 2) 
 

test_HilbertIndexInverseB :: Assertion  
test_HilbertIndexInverseB = ([5, 6]::[Integer]) @=? (fromJust $ indexToPoint (3::Int) (2::Int) (45::Integer))  


test_HilbertIndexInverse'B :: Assertion  
test_HilbertIndexInverse'B = ([minPrecision (3::Integer), minPrecision (1::Integer), minPrecision (2::Integer)]) @=?  (fromJust $ sequence (indexToPoint' (minPrecision (0::Integer)) (minPrecision (0::Integer)) (minPrecision (3::Integer)) (minPrecision (2::Integer)) [minPrecision (2::Integer),minPrecision (3::Integer), minPrecision  (1::Integer)])) 


test_HilbertIndexInverseC :: Assertion 
test_HilbertIndexInverseC = ([0, 6, 3]::[Integer]) @=? (fromJust $ indexToPoint (3) (3) 83) 

-- If the result of testHilbertIndexInverseC is [0,6,3] 
-- testHilbertIndexInverse'C needs to return [2,3,1], since: 
-- 0  6  3
-- ---------    
-- 0  1  0 | 2 
-- 0  1  1 | 3  
-- 0  0  1 | 1  
test_HilbertIndexInverse'C :: Assertion
test_HilbertIndexInverse'C = [Just (minPrecision (2::Integer)), Just (minPrecision (3::Integer)), Just (minPrecision (1::Integer))] 
                            @=? (indexToPoint' 0 0 (minPrecision (3::Integer)) (minPrecision (3::Integer)) [PrecisionNum {value = 1, precision = 3}
                                                            ,PrecisionNum {value = 2, precision = 3}
                                                            ,PrecisionNum {value = 3, precision = 3}])


-- First result list element should be 2 (prior to elements [3,1]) 
-- This is calculated using, e, d, and the first element in the converted
-- list (1, 2, 3)   
test_HilbertIndexInverse'C1a :: Assertion
test_HilbertIndexInverse'C1a = let  e = 0 
                                    d = 0 
                                    w = PrecisionNum {value = 1, precision = 3}  
                                    l = grayCode w  
                                    t = inverseTransform e d l 
                               in 
                                    (minPrecision (2::Integer)) @=? t  


-- The inverse transform step must yield 2. 
test_HilbertIndexInverse'C1aInvTransform :: Assertion
test_HilbertIndexInverse'C1aInvTransform = let e = 0 
                                               d = 0 
                                               dimension = (3::Integer) 
                                               l = fromJust (mkPrecisionNum (1::Integer) dimension)  
                                           in 
                                              (minPrecision (2::Integer)) @=? inverseTransform e d l 

-- Tail should be elements [3,1]. 
-- Again, this is calculated from the list [1,2,3]  
test_HilbertIndexInverse'C1b :: Assertion
test_HilbertIndexInverse'C1b = let e = 0 
                                   d = 0 
                                   order = 3 
                                   dimension = 3  
                                   w = PrecisionNum {value = 1, precision = 1}  
                                   ws = [PrecisionNum {value = 2, precision = 3},PrecisionNum {value = 3, precision = 3}] 
                                   e' = newE e w d  
                                   d' = newD d w dimension  
                               in 
                              [Just (minPrecision (3::Integer)), Just (minPrecision (1::Integer))] 
                              @=? indexToPoint' e' d' order dimension ws

test_HilbertIndexInverse'C2a :: Assertion
test_HilbertIndexInverse'C2a = let  e = PrecisionNum {value = 0, precision = 1}  
                                    d = PrecisionNum {value = 2, precision = 2}  
                                    w = PrecisionNum {value = 2, precision = 3}  
                                    l = grayCode w  
                                    t = inverseTransform e d l 
                                in
                                   minPrecision (3::Integer) @=? t  
                                
test_HilbertIndexInverse'C2b :: Assertion
test_HilbertIndexInverse'C2b = let  e = PrecisionNum {value = 0, precision = 1}  
                                    d = PrecisionNum {value = 2, precision = 2}  
                                    order = 3 
                                    dimension = 3  
                                    w = PrecisionNum {value = 2, precision = 3}  
                                    ws = [PrecisionNum {value =3, precision = 3}] 
                                    e' = newE e w d 
                                    d' = newD d w dimension 
                                in 
                                   [Just (minPrecision (1::Integer))] @=? indexToPoint' e' d' order dimension ws  
 
test_HilbertIndexInverse'C2newD :: Assertion
test_HilbertIndexInverse'C2newD = let d = PrecisionNum {value=2, precision=2}
                                      w = PrecisionNum {value = 2, precision = 3}
                                      dimension = 3 
                                  in
                                  (minPrecision (1::Integer)) @=?  newD d w dimension 
 
test_HilbertIndexInverse'C3a :: Assertion 
test_HilbertIndexInverse'C3a = let  e = PrecisionNum {value = 0, precision = 1}  
                                    d = PrecisionNum {value = 1, precision = 1}  
                                    w = PrecisionNum {value = 3, precision = 3}  
                                    l = grayCode w  
                                    t = inverseTransform e d l 
                                   in 
                                     (minPrecision (1::Integer))  @=? t  


test_HilbertIndexInverse'C3b :: Assertion 
test_HilbertIndexInverse'C3b = let  order = 3 
                                    dimension = 3 
                                    ws = [] 
                                    e' = PrecisionNum {value = 5, precision =3} 
                                    d' = PrecisionNum {value = 1, precision =3}  
                                   in 
                                    [ ]  @=? indexToPoint' e' d' order dimension ws  

