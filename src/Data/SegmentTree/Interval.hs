-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Interval
-- Copyright   :  (c) Dmitry Astapov 2010
-- License     :  BSD-style
-- Maintainer  :  dastapov@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple open and closed intervals
-----------------------------------------------------------------------------

module Data.SegmentTree.Interval ( R(..)
                                 , Interval(..)
                                 , Boundary(..)
                                 , subinterval, intersects, inside
                                 , merge ) where

import Text.Printf

-- | Extension of the type `v' that includes plus and minus infinity
data R v = MinusInf | R !v | PlusInf deriving (Eq, Ord)
instance Show v => Show (R v) where
  show MinusInf = "-Inf"
  show PlusInf  = "+Inf"
  show (R v) = show v

instance Bounded (R v) where
  minBound = MinusInf
  maxBound = PlusInf

-- | An interval.  The lower bound should be less than or equal to the higher bound.
data Boundary = Open | Closed deriving (Eq, Ord)
data Interval v = Interval { ltype :: ! Boundary 
                           , low :: !(R v)
                           , high :: !(R v)
                           , htype :: ! Boundary
                           } deriving (Eq, Ord)

instance Show v => Show (Interval v) where
  show (Interval o l h c) = printf "%s%s,%s%s" (opening o) (show l) (show h) (closing c)
    where
      opening Open = "("
      opening Closed = "["
      closing Open = ")"
      closing Closed = "]"
  
-- | Checks whether smaller interval is a proper subinterval of a larger interval
subinterval smaller bigger = low smaller >= low bigger && high smaller <= high bigger

-- | Checks whether two intervals intersect each other
intersects one two = low one `inside` two || high one `inside` two ||
                     low two `inside` one || high two `inside` one

-- | Checks whether point is inside the interval
inside p (Interval ltype low high htype) = (cmp ltype) low p && (cmp htype) p high
    where
      cmp Open = (<)
      cmp Closed = (<=)

-- | Merge two intervals that share a common boundary
merge i1 i2 | i1 <= i2  = Interval (ltype i1) (low i1) (high i2) (htype i2)
            | otherwise = Interval (ltype i2) (low i2) (high i1) (htype i1)

