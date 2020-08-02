{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Monoid
-- Copyright   :  (c) Dmitry Astapov 2010
-- License     :  BSD-style
-- Maintainer  :  dastapov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- Class of types "a" which could be "measured" with values from monoid "t"
--
-- Inspired by "Monoids and Finger Trees":
-- http://apfelmus.nfshost.com/articles/monoid-fingertree.html
-----------------------------------------------------------------------------
module Data.SegmentTree.Measured (Measured(..)) where

import Data.Monoid

class Monoid t => Measured a t where
    measure :: a -> t

