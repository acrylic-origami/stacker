{-# LANGUAGE LambdaCase, TupleSections #-}
module Util where

import Control.Arrow ( (***), (&&&), first, second )
import SrcLoc

import qualified Data.SegmentTree as STree
import qualified Data.SegmentTree.Interval as STInterval

import qualified Data.Map.Strict as M
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.List ( intersect )
import Data.Maybe ( catMaybes )
import HieTypes
import Lang

import qualified Data.Graph.Inductive as Gr

both f = (f *** f)
swap (a, b) = (b, a)

toNonEmpty _ (a:r) = a :| r
toNonEmpty a _ = a :| []

span2interval :: RealSrcSpan -> STInterval.Interval (Locd a)
span2interval sp = seg2interval (sp, undefined)

seg2interval :: Seg a -> STInterval.Interval (Locd a)
seg2interval seg = 
  let (l, h) = seg2intervalish seg
  in STInterval.Interval STInterval.Open (STInterval.R l) (STInterval.R h) STInterval.Open
  
seg2intervalish :: Seg a -> (Locd a, Locd a)
seg2intervalish (sp, v) = ((Locd (realSrcSpanStart sp) v), (Locd (realSrcSpanEnd sp) v))

map_loc_ivl f = catMaybes . map ((fmap f) . (\case { STInterval.R v -> Just v; otherwise -> Nothing }) . STInterval.low)
ivl_payloads = map_loc_ivl l_payload
ivl_locs = map_loc_ivl l_loc

segfind :: Segs a -> Span -> [a]
segfind (SegFlat l) sp = map snd $ filter ((`containsSpan` sp) . fst) l
segfind (SegTree t) sp = ivl_payloads $ STree.subintervalQuery t (span2interval sp)

ident_span :: Identifier -> Maybe Span
ident_span (Right n) | RealSrcSpan sp <- nameSrcSpan n = Just sp
                     | otherwise = Nothing
ident_span _ = Nothing

gr_prettify :: (Gr.DynGraph gr, Show a, Show b) => (c -> a) -> (d -> b) -> gr c d -> String
gr_prettify sa sb g = foldr (showsContext . Gr.context g) id (Gr.nodes g) ""
  where
    showsContext (_,n,l,s) sg = shows n . (':':) . shows (sa l)
                                . showString "->" . shows (map (first sb) s)
                                . ('\n':) . sg


generateShallowReferencesMap
  :: Foldable f
  => f (HieAST a)
  -> M.Map Identifier [(Span, IdentifierDetails a)]
generateShallowReferencesMap = foldr (\ast m -> M.unionWith (++) (this ast) m) M.empty
  where
    this ast = fmap (pure . (nodeSpan ast,)) $ nodeIdentifiers $ nodeInfo ast