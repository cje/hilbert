{-# LANGUAGE RankNTypes #-} 

-- | 
-- Module: Data.Algorithm.Hilbert 
-- Description: Calculate points on hilbert curve 
-- Copyright: (c) 2013-2015 CJ East 
-- License: BSD3 
-- Maintainer: CJ East <cje@ieee.org> 
-- Stability: experimental 
-- Portability: Portable 
--
--  
-- Conversion to-and-from points in a Hilbert space, based on the paper: 
--
-- Hamilton, C. /Compact Hilbert Indices - Technical Report -- CS-2006-07/ 
-- 
-- At the time of writing, the paper can be found at:
-- <https://www.cs.dal.ca/sites/default/files/technical_reports/CS-2006-07.pdf> 
-- 

module Data.Algorithm.Hilbert (
-- * Hilbert Curve Functions  
   pointToIndex
 , indexToPoint
-- * Gray Code Functions  
 , grayCode
 , grayCodeInverse
)
 where  
import Data.Algorithm.Hilbert.Functions (pointToIndex, indexToPoint, grayCode, grayCodeInverse) 

