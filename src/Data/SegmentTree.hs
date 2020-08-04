{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SegmentTree
-- Copyright   :  (c) Dmitry Astapov 2010
-- License     :  BSD-style
-- Maintainer  :  dastapov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, etc - see above)
--
-- Segment Tree implemented following section 10.3 and 10.4 of
--
--    * Mark de Berg, Otfried Cheong, Marc van Kreveld, Mark Overmars
--      "Computational Geometry, Algorithms and Applications", Third Edition
--      (2008) pp 231-237
--      \"Finger trees: a simple general-purpose data structure\",
--      /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--      <http://www.soi.city.ac.uk/~ross/papers/FingerTree.html>
--
-- Accumulation of results with monoids following "Monoids and Finger Trees", 
-- http://apfelmus.nfshost.com/articles/monoid-fingertree.html
--
-- An amortized running time is given for each operation, with /n/
-- referring to the number of intervals.
-----------------------------------------------------------------------------

module Data.SegmentTree ( R(..), Interval(..), Boundary(..), STree(..), fromList, insert, queryTree, countingQuery, stabbingQuery, subintervalQuery, superintervalQuery ) where

import Data.SegmentTree.Interval
import Data.SegmentTree.Measured
import Data.List (sort, unfoldr, foldl')
import Data.Monoid
import Text.Printf

import Control.Arrow ( (***), (&&&), first, second )

-- | Segment Tree is a binary tree that stores Interval in each leaf or branch.
-- By construction (see `leaf' and `branch') intervals in branches should be union
-- of the intervals from left and right subtrees.
--
-- Additionally, each node carries a "tag" of type "t" (which should be monoid).
-- By supplying different monoids, segment tree could be made to support different types
-- of stabbing queries: Sum or Integer monoid will give tree that counts hits, and list or
-- Set monoids will give a tree that returns actual intervals containing point.
data STree t a = Leaf   !t !(Interval a)
               | Branch !t !(Interval a) !(STree t a) !(STree t a)
                          
instance (Show t, Show a) => Show (STree t a) where
  show (Leaf t i) = printf "Leaf %s %s" (show t) (show i)
  show (Branch t i left right) = printf "Branch %s %s (\n  %s\n  %s)" (show t) (show i) (show left) (show right)
                
-- Selectors for STree
tag :: STree t a -> t
tag (Leaf t _)       = t
tag (Branch t _ _ _) = t

interval (Leaf _ i) = i
interval (Branch _ i _ _) = i

-- Constructors for STree nodes
branch :: (Ord a, Measured (Interval a) t) => STree t a -> STree t a -> STree t a
branch x y = Branch (tag x `mappend` tag y) (merge (interval x) (interval y)) x y

leaf :: (Ord a, Measured (Interval a) t) => Interval a -> STree t a
leaf a = Leaf (measure a) a

-- Instances that allow creation of useful trees.
--
-- Trees for stabbing count queries:
-- @
-- STree Integer Rational
-- STree (Sum Integer) Rational
-- @
--
-- Trees for stabbing queries:
-- @
-- STree [Interval Rational] Rational
-- STree (Set (Interval Rational)) Rational
-- @

-- instance Measured (Interval a) [Interval a] where
--   measure x = [x]

instance (Num a, Num b) => Measured (Interval a) (Sum b) where
  measure _ = Sum 1

-- instance Monoid Integer where
--   mempty = 0
--   mappend = (+)

-- | Build the 'SegmentTree' for the given list of pair of points. Time: O(n*log n)
-- Segment tree is built as follows:
--  * Supplied list of point pairs define so-called "atomic intervals"  
--  * They are used to build "skeleton" binary tree
--  * Each supplied interval is then "inserted" into this tree, updating tag values 
--    in tree branches and leaves
fromList :: (Monoid t, Measured (Interval a) t, Ord a) => [(a,a)] -> STree t a
fromList pairs = foldl' insert skeleton intervals
  where 
    -- "intervals" is just an original list of pairs converted to "Interval" datatype
    intervals = map pair2interval pairs
    pair2interval (a,b) = Interval Closed (R a) (R b) Closed
    
    -- "skeleton" tree is a binary tree where each leaf holds some atomic interval (and empty tag)
    -- and each branch holds union of intervals from its leaves (and empty tag).
    -- Tree is built from bottom up, by making "leaves" first and then connecting them with branches
    -- pairwise, until a single root is obtained.
    ([skeleton]:_) = dropWhile (not.converged) $ iterate (unfoldr connect) leaves    
    leaves = map (Leaf mempty) atomics
    connect []         = Nothing
    connect [x,y,z]    = Just $ ((x `branch` y) `branch` z, [])
    connect (x:y:rest) = Just $ (x `branch` y, rest)
    converged [x] = True
    converged _   = False
    
    -- Open "atomic" intervals are formed between the (sorted) endpoints of original intervals.
    -- Leftmost atomic interval starts from minu infinity, rightmost ends with infinity.
    -- All endpoints are also converted to closed single-point atomic intervals.
    -- For details, see book referenced above or wikipedia.
    atomics = concat (zipWith atomicInterval endpoints (drop 1 endpoints))
    atomicInterval a PlusInf = [Interval Open a PlusInf Open]
    atomicInterval a b       = [Interval Open a b       Open, Interval Closed b b Closed]
    endpoints = sort $ foldl' (\acc i -> (low i):(high i):acc) [MinusInf,PlusInf] intervals
    
-- | Insert interval `i' into segment tree, updating tag values as necessary.
-- Semantics of tags depends on the monoid used (see `fromList')
insert :: (Ord a, Measured (Interval a) t) => STree t a -> Interval a -> STree t a
insert leaf@(Leaf t iu) i
  | iu `subinterval` i = Leaf (t `mappend` (measure i)) iu
  | otherwise       = leaf
insert (Branch t iu left right) i
  | iu `subinterval` i = Branch (t `mappend` (measure i)) iu left right
  | otherwise = 
      let left' = if i `intersects` (interval left) then insert left i else left 
          right' = if i `intersects` (interval right) then insert right i else right
          in Branch t iu left' right'

-- | Query the segment tree for the specified point. Time: O(log n)
queryTree :: (Monoid t, Measured (Interval a) t, Ord a) => STree t a -> a -> t
queryTree t point = go t (R point)
  where
    go (Leaf t ivl) point 
      | point `inside` ivl = t
      | otherwise = mempty
    go (Branch t ivl left right) point = t `mappend` qleft `mappend` qright
      where 
        qleft  = if point `inside` (interval left)  then go left  point else mempty
        qright = if point `inside` (interval right) then go right point else mempty

superintervalQuery :: (Monoid t, Ord a) => STree t a -> Interval a -> t
superintervalQuery t i =
  let ivl = interval t
      filt = if ivl `subinterval` i then id else const mempty
  in case t of
    Leaf u _ -> filt u
    Branch u _ l r -> filt u <> superintervalQuery l i <> superintervalQuery r i 
    
subintervalQuery :: (Monoid t, Ord a) => STree t a -> Interval a -> t
subintervalQuery t i | ivl <- interval t
                     , i `subinterval` ivl
                     = case t of
                       Leaf u _ -> u
                       Branch u _ l r -> u <> subintervalQuery l i <> subintervalQuery r i
                     | otherwise = mempty

-- | Convenience wrapper around `queryTree'. Returns count of intervals covering the `point'
countingQuery :: (Ord a, Num a, Num b) => STree (Sum b) a -> a -> b
countingQuery tree point = getSum (queryTree tree point)

-- | Convenience wrapper around `queryTree' to perform stabbing query. Returns list of intevals coverting the point
stabbingQuery :: (Measured (Interval a) [Interval a], Ord a) => STree [Interval a] a -> a -> [Interval a]
stabbingQuery = queryTree

-- | Convenience wrapper around `queryTree' to perform stabbing query. Returns set of intevals coverting the point
-- stabbingSetQuery :: (Measured (Interval a) (Set (Interval a)), Ord a) => STree (Set (Interval a)) a -> a -> Set (Interval a)
-- stabbingSetQuery = queryTree
