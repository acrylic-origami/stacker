{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, TemplateHaskell, LambdaCase, TupleSections, Rank2Types, MultiWayIf, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Lang where

import Control.Lens ( makeLenses )
import Control.Lens.Operators

import Control.Arrow ( (***), (&&&), first, second )
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Tree as Tr
import Data.List ( intersect )

import qualified GHC.Prof as Prof

import HieBin
import HieTypes
import HieUtils

import SrcLoc

import qualified Data.SegmentTree as STree
import Data.SegmentTree.Measured
import Data.SegmentTree.Interval ( Interval(..) )

import Data.Graph.Inductive

import Outputable ( Outputable(..), (<+>), ($+$) )
import qualified Outputable as O

data Locd a = Locd {
  l_loc :: RealSrcLoc,
  l_payload :: a
} deriving Show

instance Eq (Locd a) where
  (Locd l _) == (Locd r _) = l == r
instance Ord (Locd a) where
  (Locd l _) `compare` (Locd r _) = l `compare` r
instance Functor Locd where
  f `fmap` (Locd l a) = Locd l (f a)

data Spand a = Spand {
  s_span :: RealSrcSpan,
  s_payload :: a
} deriving Show

instance Eq (Spand a) where
  (Spand l _) == (Spand r _) = l == r
instance Ord (Spand a) where
  (Spand l _) `compare` (Spand r _) = l `compare` r
  
type Seg a = (Span, a)
data Segs a = SegFlat [Seg a] | SegTree (STree.STree [Interval (Locd a)] (Locd a)) deriving Show

instance Semigroup (Segs a) where
  (SegFlat l) <> (SegFlat r) = SegFlat (l <> r)
  _ <> _ = error "Unmergeable Segs" -- mm.
instance Monoid (Segs a) where
  mempty = SegFlat []

type AppGroup = Spand [Identifier]

data AppGroups a = AppGroups {
  _ag_ident_map :: M.Map Identifier [AppGroup], -- app groups are disjoint sets, but all libs are too annoying to use (e.g. there aren't any nice and easy ele -> class functions) so just make them all point to the same set by construction
  _ag_span_map :: Segs AppGroup
}
makeLenses ''AppGroups

type IdentMap a = M.Map Identifier [(Span, IdentifierDetails a)]
type IdentBindMap a = M.Map Identifier BindKey

data BindKey = BindNamed Span Identifier | BindLam Span

instance Eq BindKey where
  (BindNamed _ l) == (BindNamed _ r) = l == r
  (BindLam l) == (BindLam r) = l == r
  _ == _ = False

instance Ord BindKey where
  (BindNamed _ l) `compare` (BindNamed _ r) = l `compare` r
  (BindLam l) `compare` (BindLam r) = l `compare` r
  (BindNamed _ _) `compare` (BindLam _) = LT
  (BindLam _) `compare` (BindNamed _ _) = GT

fnkey_span :: BindKey -> Span
fnkey_span (BindNamed sp _) = sp
fnkey_span (BindLam sp) = sp

data PtStore a = PtStore {
  _ps_app_groups :: AppGroups a, -- app groups: maps of spans and identifiers to disjoint sets of app groups
  -- _ps_cstree :: Segs (Maybe Prof.CostCentre), -- tree of coverage of cost center stacks
  _ps_binds :: IdentBindMap a -- map from binders to bindees
}
makeLenses ''PtStore

data NodeKey a = NKApp AppGroup | NKBind BindKey deriving (Eq, Ord)
-- type NodeStore a = BM.Bimap (NodeKey a) Node

-- data NodeStore a = NodeStore {
--   ns_app_groups :: M.Map AppGroup Node,
--   ns_binds :: M.Map (BindGroup a) Node,
--   ns_fns :: M.Map BindKey Node
-- }

-- for BindPt, collapse IdentBindMap to RHS binds in AppGroups
-- data Pt a = AppPt AppGroup | BindPt AppGroup | FnPt BindKey deriving (Eq, Ord)

-- data PatGroup t = PG {
--   pg_from :: t Identifier,
--   pg_to :: t Identifier
-- } -- deriving (Semigroup, Monoid)

instance Monoid t => Measured t t where
  measure = id

instance (Applicative t, Monoid (t a)) => Measured a (t a) where
  measure = pure

instance Semigroup (AppGroups a) where
  (AppGroups la lb) <> (AppGroups ra rb) = AppGroups (M.unionWith (<>) la ra) (lb <> rb)
  -- error "AppGroup is not mergeable" -- note: plain union vs unionWith due to no sensible way to merge Spans of AppGroups, which should be disjoint and predetermined
instance Monoid (AppGroups a) where
  mempty = AppGroups mempty mempty
instance Semigroup (PtStore a) where
  (PtStore la lb) <> (PtStore ra rb) = PtStore (la <> ra) (lb <> rb)
instance Monoid (PtStore a) where
  mempty = PtStore mempty mempty
  
instance Outputable a => Outputable (Locd a) where
  ppr (Locd l a) = O.text "@" <+> ppr l <+> O.colon $+$ ppr a
  
instance Outputable a => Outputable (Spand a) where
  ppr (Spand l a) = O.text "@" <+> ppr l <+> O.colon $+$ ppr a

instance Outputable (AppGroups a) where
  ppr (AppGroups m _) = ppr m

instance Outputable BindKey where
  ppr (BindNamed s i) = O.text "BindNamed" <+> ppr i <+> O.text "@" O.<> ppr s
  ppr (BindLam s) = O.text "BindLam" <+> ppr s

instance Outputable (PtStore a) where
  ppr (PtStore {..}) = O.braces $
      ppr _ps_app_groups
      <+> ppr _ps_binds

instance Outputable (NodeKey a) where
  ppr (NKApp a) = O.text "NKApp" <+> ppr a
  ppr (NKBind b) = O.text "NKBind" <+> ppr b
