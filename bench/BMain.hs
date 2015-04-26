-- Based on upon Jan Stolarek's Haskell Testing Stub, 
-- https://github.com/jstolarek/haskell-testing-stub
module Main where

import System.Random 
import Criterion.Main 
import Criterion.Types 
import BHilbert 

main :: IO () 
main = defaultMainWith benchmarkConfig . benchmarks $ mkStdGen 42 

benchmarks :: RandomGen g => g -> [Benchmark] 
benchmarks gen = [
 bgroup "Benchmarks" 
     [
       bench "Hilbert Index Inverse" $ nf bench_indexToPoint (take 1000 $ randoms gen )  
     , bench "Hilbert Index" $ nf bench_pointToIndex $ replicate 100 (take 10 $ randoms gen) 
     ] 

 ] 

benchmarkConfig :: Config 
benchmarkConfig = defaultConfig { 
                  forceGC = True 
                  } 

