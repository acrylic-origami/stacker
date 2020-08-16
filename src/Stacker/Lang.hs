{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, TemplateHaskell, LambdaCase, TupleSections, Rank2Types, MultiWayIf, DeriveGeneric, FlexibleInstancesm, MultiParamTypeClasses #-}
module Stacker.Lang where

import Control.Lens ( makeLenses )
import Control.Lens.Operators
import GHC.Generics

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

import qualified Data.Graph.Inductive as Gr

import Data.Aeson ( FromJSON(..), ToJSON(..) )
import qualified Data.Aeson as Aeson

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

instance Eq a => Eq (Spand a) where
  (Spand _ l) == (Spand _ r) = l == r
instance Ord a => Ord (Spand a) where
  (Spand _ l) `compare` (Spand _ r) = l `compare` r
  
type LIdentifier = Spand Identifier
  
data Segs a = SegFlat [Spand a] | SegTree (STree.STree [Interval (Locd a)] (Locd a)) deriving Show

instance Semigroup (Segs a) where
  (SegFlat l) <> (SegFlat r) = SegFlat (l <> r)
  _ <> _ = error "Unmergeable Segs" -- mm.
instance Monoid (Segs a) where
  mempty = SegFlat []

type AppGroup = Spand [LIdentifier]

data AppGroups a = AppGroups {
  _ag_ident_map :: M.Map LIdentifier [AppGroup], -- app groups are disjoint sets, but all libs are too annoying to use (e.g. there aren't any nice and easy ele -> class functions) so just make them all point to the same set by construction
  _ag_span_map :: Segs AppGroup
} -- NOTE! AppGroups are used to find instances of _usages_ of _bindings_ after tracing an ident to an _argument_ only. NOT for idents -> other idents. Instead trace those to their binding sites, then to their RHS closed dependencies
makeLenses ''AppGroups

data BindKey = BindNamed LIdentifier | BindLam Span deriving (Eq, Ord)

type IdentMap a = M.Map LIdentifier [(Span, IdentifierDetails a)]
data BindGroups = BindGroups {
  _bg_arg_bnd_map :: M.Map LIdentifier BindKey,
  _bg_bnd_app_map :: M.Map LIdentifier [AppGroup]
  -- _bg_named_lookup :: S.Set LIdentifier -- find BindNamed
}
makeLenses ''BindGroups

instance Semigroup BindGroups where
  (BindGroups la lb) <> (BindGroups ra rb) = BindGroups (la <> ra) (M.unionWith (<>) lb rb)
instance Monoid BindGroups where
  mempty = BindGroups mempty mempty

data PtStore a = PtStore {
  _ps_app_groups :: AppGroups a, -- app groups: maps of spans and identifiers to disjoint sets of app groups
  -- _ps_cstree :: Segs (Maybe Prof.CostCentre), -- tree of coverage of cost center stacks
  _ps_binds :: BindGroups -- map from binders to bindees
}
makeLenses ''PtStore

data NodeKey a = NKApp AppGroup | NKBind BindKey deriving (Eq, Ord)

nk_span :: NodeKey a -> Span
nk_span (NKApp ag) = s_span ag
nk_span (NKBind (BindNamed lident)) = s_span lident
nk_span (NKBind (BindLam sp)) = sp
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
  ppr (BindNamed i) = O.text "BindNamed" <+> ppr i
  ppr (BindLam s) = O.text "BindLam" <+> ppr s
  
instance Outputable BindGroups where
  ppr (BindGroups {..}) = O.text "BindGroups" $+$ ppr _bg_arg_bnd_map $+$ ppr _bg_bnd_app_map

instance Outputable (PtStore a) where
  ppr (PtStore {..}) = O.braces $
      ppr _ps_app_groups
      <+> ppr _ps_binds

instance Outputable (NodeKey a) where
  ppr (NKApp a) = O.text "NKApp" <+> ppr a
  ppr (NKBind b) = O.text "NKBind" <+> ppr b

--------------------------------------------------

-- OUTPUT INTERFACE --

type AdjList k a b = M.Map k (a, [(k, b)]) -- [(k, (a, [(k, b)]))]
type NodeKeyCtor = String
type HollowLoc = (Int, Int)
type HollowSpan = (HollowLoc, HollowLoc)
type HollowKey = (Int, Gr.Node) -- hacks to support multiple graphs without having to reindex all the nodes
data JSGraph = JSGraph {
  jsg_gr :: AdjList HollowKey (NodeKeyCtor, HollowSpan) HollowSpan
  , jsg_sccs :: [[HollowKey]]
} deriving Generic
data HollowGrState = HollowGrState {
  st_at :: [HollowKey]
  , st_gr :: JSGraph
} deriving Generic

gr2adjlist :: Ord k => (Gr.Node -> k) -> Gr.Gr a b -> AdjList k a b
gr2adjlist k gr = M.fromList $ map ((k &&& (Gr.lab' &&& map (first k) . Gr.lsuc') . Gr.context gr)) (Gr.nodes gr)

hollow_loc = srcLocLine &&& srcLocCol
hollow_span = (hollow_loc . realSrcSpanStart &&& hollow_loc . realSrcSpanEnd)

nk_ctor :: NodeKey a -> NodeKeyCtor
nk_ctor (NKBind _) = "NKBind"
nk_ctor (NKApp _) = "NKApp"

instance Semigroup JSGraph where
  (JSGraph la lb) <> (JSGraph ra rb) = JSGraph (la <> ra) (lb <> rb)
  
instance Monoid JSGraph where
  mempty = JSGraph mempty mempty
  
instance ToJSON JSGraph where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
  
instance ToJSON HollowGrState where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions