{-# LANGUAGE ScopedTypeVariables #-}
module Main where 
import THilbert
--import Test.HUnit
--import Test.QuickCheck
import Test.Framework 
import Test.Framework.Providers.HUnit  
import Test.Framework.Providers.QuickCheck2

main :: IO ()  
main = defaultMain hilbertTests  

hilbertTests :: [Test] 
hilbertTests = [ 
               testGroup "Type / Instance / Utility" 
               [
                 testProperty "Bool / Num Bijective (small)" prop_boolNumBijectiveSmall 
               , testProperty "Bool / Num Bijective (large)" prop_boolNumBijectiveLarge 
               , testCase     "Number to Bool list"  test_NumToBool
               , testCase     "Bool list to Number"  test_BoolToNum

               , testCase     "convertPointToHypercube (A)" test_ConvertPointToHypercubeA
               , testCase     "convertPointToHypercube (B)" test_ConvertPointToHypercubeB

               , testCase     "convertInteger (A)"  test_ConvertIntegerA
               , testCase     "convertInteger (B)" test_ConvertIntegerB 
               , testProperty "Convert Integer yields equal pieces" prop_ConvertIntegerEqualPieces 


               , testProperty "RotateL-RotateR bijective" prop_rotationsBijective 
               , testCase     "RotateL (A)" test_RotateLmodnA
               , testCase     "RotateL (B)" test_RotateLmodnB
               , testCase     "RotateL (C)" test_RotateLmodnC
               , testCase     "RotateL (D)" test_RotateLmodnD
               , testCase     "RotateR (A)" test_RotateRmodnA
               , testCase     "RotateR (B)" test_RotateRmodnB

               , testProperty "Precision represents" prop_precisionRepresents 
               , testCase     "Precision (A)"  test_PrecisionRequiredA 
               , testCase     "Precision (B)"  test_PrecisionRequiredB 
               , testCase     "Precision (C)"  test_PrecisionRequiredC 
               , testCase     "mkPrecisionNum" test_MkPrecisionNum 

               , testCase     "Num instance"  test_Num  

               , testCase     "shiftRALarge"  test_ShiftRALarge 
               , testCase     "ShiftRASmall" test_ShiftRASmall 
               , testCase     "ShiftRAPrecision" test_ShiftRAPrecision 
               , testCase     "ShiftLASmall" test_ShiftLASmall 
               , testCase     "ShiftLAPrecision" test_ShiftLAPrecision  
                              ],  
               testGroup "Utility Functions"
               [ 
                 testProperty "Successor bit determined by trailingSetBits" prop_SuccessorBitByTrailingBits   
               , testProperty "trailingSetBits is Symmetric"  prop_trailingSetBitsSymmetric
               , testProperty "entryPointExitPoint is Symmetric" prop_entryPointExitPointSymmetric 
               , testProperty "Direction symmetric" prop_directionSymmetric 
               , testProperty "Relationship between entry, direction, exit" prop_entryDirectionExit
               , testProperty "Direction When i=0 is 0" prop_directionWhenIZero
               , testProperty "Direction is same if one dimension higher" prop_directionOneHigherDimension
               , testProperty "Transform function bijective" prop_transformBijective 
               , testCase     "EntryPoint (A)"  test_EntryPointA 
               , testCase     "EntryPoint (B)"  test_EntryPointB 
               , testCase     "EntryPoint (C)"  test_EntryPointC 
               , testCase     "EntryPoint (D)"  test_EntryPointD 
               , testCase     "ExitPoint (A)"  test_ExitPointA 
               , testCase     "ExitPoint (B)"  test_ExitPointB 
               , testCase     "ExitPoint (C)"  test_ExitPointC 
               , testCase     "ExitPoint (D)"  test_ExitPointD 
               , testCase     "transform (A)"      test_TransformA
               , testCase     "transform (B)"      test_TransformB
               , testCase     "transform (C)"      test_TransformC
               , testCase     "transform (D)"      test_TransformD
               , testCase     "inverseTransform (A)" test_InverseTransformA 
               , testCase     "inverseTransform (B)" test_InverseTransformB 
               , testCase     "inverseTransform (C)" test_InverseTransformC
               , testCase     "inverseTransform (D)" test_InverseTransformD 
               , testCase     "inverseTransform (E)" test_InverseTransformE
               , testCase     "text_Exp" test_Exp 
               , testCase     "test_Direction (A)"  test_DirectionA
               , testCase     "test_Direction (B)"  test_DirectionB 

               ], 
               testGroup "Gray code tests" 
               [
                 testProperty "Gray code is bijective" prop_graycodeBijective 
               , testCase     "Graycode (A) test"  test_GrayCodeA 
               , testCase     "Graycode (B) test"  test_GrayCodeB 
               , testCase     "Graycode (C) test"  test_GrayCodeC 
               , testCase     "Graycode (D) test"  test_GrayCodeD 
               , testProperty "grayCodeSymmetric"  prop_grayCodeSymmetric 
               , testProperty "grayCodeSymmetricLarge" prop_grayCodeSymmetricLarge
               , testProperty "Gray code maintains precision" prop_grayCodeMaintainsPrecision
               , testCase     "iGrayCode1"     test_iGrayCode1
               , testCase     "iGrayCode2"     test_iGrayCode2
               , testCase     "iGrayCode3"     test_iGrayCode3

               ], 
               testGroup "Hilbert tests"
               [ 
                 testProperty "Order 32, Dimension 32 Hilbert Transform / Untransform" prop_hilbertBijectiveO32D32 
               , testProperty "Hilbert Transform bijective" prop_hilbertBijective 
               , testProperty "Hilbert Exhaustive over range" prop_hilbertExhaustive 
               , testCase     "test_HilbertExhaustive" test_HilbertExhaustive 
               , testCase     "test_SetTrailingBits" test_SetTrailingBits
               , testCase     "test_SetTrailingBits2" test_SetTrailingBits2
               , testCase     "test_SetTrailingBits3" test_SetTrailingBits3
               , testCase     "test_SetTrailingBits4" test_SetTrailingBits4
               , testCase     "test_SetTrailingBits5" test_SetTrailingBits5 
               , testCase     "test_TransformRef1" test_TransformRef1 
               , testCase     "test_TransformRef2" test_TransformRef2 
               , testCase     "test_TransformRef3" test_TransformRef3 
               , testCase     "test_WRef1" test_WRef1
               , testCase     "test_WRef2" test_WRef2
               , testCase     "test_WRef3" test_WRef3
               , testCase     "test_ERef1" test_ERef1 
               , testCase     "test_ERef2" test_ERef2 
               , testCase     "test_ERef3" test_ERef3 
               , testCase     "test_DRef1" test_DRef1   
               , testCase     "test_DRef2" test_DRef2   
               , testCase     "test_DRef3" test_DRef3  

               , testCase     "test_HilbertIndex"  test_HilbertIndex
               , testCase     "test_Stage1overall" test_Stage1overall 
               , testCase     "test_Stage1overallPrecision" test_Stage1overallPrecision 
               , testCase     "test_Stage1ShiftL" test_Stage1ShiftL  
               , testCase     "test_Stage1t" test_Stage1t 
               , testCase     "test_Stage1tPrecision" test_Stage1tPrecision 
               , testCase     "test_Stage1w" test_Stage1w 
               , testCase     "test_Stage1wPrecision" test_Stage1wPrecision 
               , testCase     "test_Stage1e'" test_Stage1e'
               , testCase     "test_Stage1e'Precision" test_Stage1e'Precision
               , testCase     "test_Stage1d'" test_Stage1d'
               , testCase     "test_Stage1d'Precision" test_Stage1d'Precision
               , testCase     "test_Stage2overall" test_Stage2overall 
               , testCase     "test_Stage2t" test_Stage2t  
               , testCase     "test_Stage2w" test_Stage2w 
               , testCase     "test_Stage2e'" test_Stage2e'
               , testCase     "test_stage2d'" test_Stage2d'
               , testCase     "test_Stage3overall" test_Stage3overall
               , testCase     "test_Stage3transform" test_Stage3transform
               , testCase     "test_Stage3transformDim" test_Stage3transformDim
               , testCase     "test_Stage3transformXor" test_Stage3transformXor 
               , testCase     "test_Stage3transformRotateR" test_Stage3transformRotateR 
               , testCase     "test_Stage3w" test_Stage3w 
               , testCase     "test_Stage3e'" test_Stage3e' 
               , testCase     "test_Stage3d'" test_Stage3d' 
               , testCase     "test_HilbertIndex2"  test_HilbertIndex2
               , testCase     "test_HilbertIndex3"  test_HilbertIndex3
               , testCase     "test_HilbertIndex4"  test_HilbertIndex4
               , testCase     "test_HilbertIndexInverse" test_HilbertIndexInverse
               , testCase     "test_HilbertIndexInverse'" test_HilbertIndexInverse'
	       , testCase     "test_HilbertIndexInverseGrayCode" test_HilbertIndexInverseGrayCode 
	       , testCase     "test_HilbertIndexInverseTransform" test_HilbertIndexInverseInverseTransform 
	       , testCase     "test_HilbertIndexInverseNewE" test_HilbertIndexInverseNewE 
	       , testCase     "test_HilbertIndexInverseNewD" test_HilbertIndexInverseNewD 
               , testCase     "test_HilbertIndexInverseB" test_HilbertIndexInverseB
               , testCase     "test_HilbertIndexInverse'B" test_HilbertIndexInverse'B
               , testCase     "test_HilbertIndexInverseC" test_HilbertIndexInverseC 
               , testCase     "test_HilbertIndexInverse'C" test_HilbertIndexInverse'C 
               , testCase     "test_HilbertIndexInverse'C1a" test_HilbertIndexInverse'C1a 
               , testCase     "test_HilbertIndexInverse'C1aInvTransform" test_HilbertIndexInverse'C1aInvTransform 
               , testCase     "test_HilbertIndexInverse'C1b" test_HilbertIndexInverse'C1b 
               , testCase     "test_HilbertIndexInverse'C2a" test_HilbertIndexInverse'C2a 
               , testCase     "test_HilbertIndexInverse'C2newD" test_HilbertIndexInverse'C2newD 
               , testCase     "test_HilbertIndexInverse'C3a" test_HilbertIndexInverse'C3a 
               , testCase     "test_HilbertIndexInverse'C3b" test_HilbertIndexInverse'C3b 

                              ]
              ] 
              

        

