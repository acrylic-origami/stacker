{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, TemplateHaskell, LambdaCase, TupleSections, Rank2Types, MultiWayIf, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
module Stacker.Lang where

import Control.Lens ( makeLenses )
import qualified Control.Lens.Operators as LOp
import GHC.Generics
import qualified FastString as FS
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Control.Arrow ( (***), (&&&), first, second )
import qualified Data.Set as S
import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Bimap as BM
import qualified Data.Tree as Tr
import Data.List ( intersect )
import Control.Monad.State ( State(..) )

import qualified GHC.Prof as Prof

import HieBin
import HieTypes
import HieUtils

import SrcLoc

import qualified Data.SegmentTree as STree
import Data.SegmentTree.Measured
import Data.SegmentTree.Interval ( Interval(..) )

import qualified Data.Graph.Inductive as Gr

import Data.Aeson ( FromJSON(..), ToJSON(..), ToJSONKey(..), (.=) )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

import Outputable ( Outputable(..), (<+>), ($+$) )
import qualified Outputable as O

data Locd a = Locd {
  l_loc :: RealSrcLoc,
  l_payload :: a
} deriving (Show)

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
bk_span (BindLam sp) = sp
bk_span (BindNamed b) = s_span b

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

data EdgeLabel =
  ArgEdge LIdentifier LIdentifier -- loc of source, loc of target argument
  | AppEdge LIdentifier LIdentifier -- loc of source, loc of target binding
  | BindEdge Span -- just the bindee location itself
  -- the name location that links the nodes together (e.g. a symbol within an AppGroup + its binding, the arg )
type NodeGr a = Gr.Gr ((NodeKey a, Int)) EdgeLabel
type NodeState a = State (S.Set AppGroup) ([Gr.Node], [Gr.LEdge EdgeLabel])

-- OUTPUT INTERFACE --

hollow_loc = srcLocLine &&& srcLocCol

instance ToJSON RealSrcSpan where
  toJSON sp = toJSON (
      FS.unpackFS $ srcSpanFile sp
      , hollow_loc $ realSrcSpanStart sp
      , hollow_loc $ realSrcSpanEnd sp
    )

instance ToJSONKey RealSrcSpan where
  toJSONKey = Aeson.Types.toJSONKeyText (E.decodeUtf8 . LBS.toStrict . Aeson.encode)

instance ToJSON (Spand a) where
  toJSON (Spand sp _) = toJSON sp

-- :(
nk_ctor :: NodeKey a -> String
nk_ctor (NKBind _) = "NKBind"
nk_ctor (NKApp _) = "NKApp"
bk_ctor :: BindKey -> String
bk_ctor (BindNamed _) = "BindNamed"
bk_ctor (BindLam _) = "BindLam"
el_ctor :: EdgeLabel -> String
el_ctor (ArgEdge _ _) = "ArgEdge"
el_ctor (AppEdge _ _) = "AppEdge"
el_ctor (BindEdge _) = "BindEdge"

defTagField = T.pack $ Aeson.tagFieldName $ Aeson.sumEncoding Aeson.defaultOptions
defContentsField = T.pack $ Aeson.contentsFieldName $ Aeson.sumEncoding Aeson.defaultOptions
-- fake the impls of the *Key's because the Identifier and Span can't be made Generic from GHC
instance ToJSON (NodeKey a) where
  toJSON k = Aeson.object [
      defTagField .= nk_ctor k
      , (defContentsField, case k of { NKBind n -> toJSON n; NKApp n -> toJSON n })
    ]

instance ToJSON BindKey where
  toJSON k = Aeson.object [
      defTagField .= bk_ctor k
      , (defContentsField, case k of { BindNamed b -> toJSON b; BindLam b -> toJSON b })
    ]
  
instance ToJSON EdgeLabel where
  toJSON l = Aeson.object [
      defTagField .= el_ctor l
      , (defContentsField, case l of { ArgEdge a b -> toJSON (a, b); AppEdge a b -> toJSON (a, b); BindEdge a -> toJSON a })
    ]

type AdjList a b = IM.IntMap (a, [(Gr.Node, b)]) -- [(k, (a, [(k, b)]))]
-- type SrcFileMap = M.Map FilePath Int
data HollowGrState a = HollowGrState {
  st_at :: [Gr.Node]
  -- , jsg_sourcelist :: [String]
  , st_gr :: AdjList (NodeKey a, Int) EdgeLabel
}
  
instance Semigroup (HollowGrState a) where
  (HollowGrState la lb) <> (HollowGrState ra rb) = HollowGrState (la <> ra) (lb <> rb)

instance Monoid (HollowGrState a) where
  mempty = HollowGrState mempty mempty
  
instance ToJSON (HollowGrState a) where
  toJSON (HollowGrState a b) = toJSON (a, b)

gr2adjlist :: Gr.Gr a b -> AdjList a b
gr2adjlist gr = Gr.ufold (\c m -> IM.insert (Gr.node' c) (Gr.lab' c, Gr.lsuc' c) m) mempty gr
  -- M.fromList $ map ((id &&& (Gr.lab' &&& map (first k) . Gr.lsuc') . Gr.context gr)) (Gr.nodes gr)

-- instance Semigroup JSGraph where
--   (JSGraph la lb) <> (JSGraph ra rb) = JSGraph (la <> ra) (lb <> rb)
  
-- instance Monoid JSGraph where
--   mempty = JSGraph mempty mempty
  
-- instance ToJSON JSGraph where
--   toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
