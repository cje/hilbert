module BHilbert where
import Data.Algorithm.Hilbert


bench_pointToIndex :: [[Integer]] -> [Maybe Integer]  
bench_pointToIndex pointList =  let order = 32  
                                    dimension = 32 
                                    valA = (fmap . fmap) abs pointList  
                                    test = pointToIndex order dimension   
                                  in 
                                     map test valA  

bench_indexToPoint :: [Int] -> [Maybe [Integer]]  
bench_indexToPoint val =  let order = 32  
                              dimension = 32 
                              valA = map abs val  
                              invTest x = indexToPoint order dimension (fromIntegral x)
                            in 
                              map invTest valA  
