{-# LANGUAGE LambdaCase, TupleSections #-}
module Stacker.Util where

import Control.Arrow ( (***), (&&&), first, second )
import Control.Lens ( lens, sets, to, Setter(..), Setter'(..), Lens' )
import Control.Lens.Operators ( (^.), (%~), (.~) )
import Data.Function ( (&) )

import SrcLoc
import Name ( nameSrcSpan )
import FastString ( FastString(..) )

import qualified Data.SegmentTree as STree
import qualified Data.SegmentTree.Interval as STInterval

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bitraversable ( bisequence )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.List ( intersect )
import Data.Maybe ( catMaybes )
import HieTypes

import Stacker.Lang

import Outputable ( showSDoc, interppSP, Outputable(..), (<+>), ($+$) )
import qualified Outputable as O
import DynFlags ( DynFlags )
import Name ( nameOccName, nameModule_maybe, Name(..) )
import OccName ( occNameString )

import qualified Data.Graph.Inductive as Gr

import Debug.Trace ( trace, traceShow )

import SysTools ( initSysTools )
import DynFlags ( DynFlags, defaultDynFlags )
import GHC.Paths (libdir)
import NameCache ( NameCache(..), initNameCache )
import UniqSupply ( mkSplitUniqSupply )

dynFlagsForPrinting :: IO DynFlags
dynFlagsForPrinting = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings ([], [])
  
makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []
  
both :: (a -> b) -> ((a, a) -> (b, b))
swap :: (a, b) -> (b, a)
dupe :: a -> (a, a)
both f = (f *** f)
swap (a, b) = (b, a)
dupe a = (a, a)

lassoc (a, (b, c)) = ((a, b), c)
rassoc ((a, b), c) = (a, (b, c))

unique :: (Eq a, Ord a) => [a] -> [a]
unique = S.toList . S.fromList

toNonEmpty _ (a:r) = a :| r
toNonEmpty a _ = a :| []

span2interval :: RealSrcSpan -> STInterval.Interval (Locd a)
span2interval sp = seg2interval (Spand sp undefined)

seg2interval :: Spand a -> STInterval.Interval (Locd a)
seg2interval sp = 
  let (l, h) = seg2intervalish sp
  in STInterval.Interval STInterval.Open (STInterval.R l) (STInterval.R h) STInterval.Open
  
seg2intervalish :: Spand a -> (Locd a, Locd a)
seg2intervalish (Spand sp v) = ((Locd (realSrcSpanStart sp) v), (Locd (realSrcSpanEnd sp) v))

map_loc_ivl f = catMaybes . map ((fmap f) . (\case { STInterval.R v -> Just v; otherwise -> Nothing }) . STInterval.low)
ivl_payloads = map_loc_ivl l_payload
ivl_locs = map_loc_ivl l_loc

segfind :: Segs a -> Span -> [a]
segfind (SegFlat l) sp = map _s_payload $ filter ((`containsSpan` sp) . _s_span) l
segfind (SegTree t) sp = ivl_payloads $ STree.subintervalQuery t (span2interval sp)

gr_prettify :: (Gr.DynGraph gr, Show a, Show b) => (c -> a) -> (d -> b) -> gr c d -> String
gr_prettify sa sb g = foldr (showsContext . Gr.context g) id (Gr.nodes g) ""
  where
    showsContext (_,n,l,s) sg = shows n . (':':) . shows (sa l)
                                . showString "->" . shows (map (first sb) s)
                                . ('\n':) . sg

passtrace :: Show a => a -> a
passtrace = uncurry traceShow . dupe

traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

ppr_safe :: Outputable a => DynFlags -> a -> String
ppr_safe d = showSDoc d . interppSP . pure

ppr_ :: Outputable a => DynFlags -> a -> IO ()
ppr_ d = putStrLn . ppr_safe d

ppr_nk :: DynFlags -> NodeKey a -> String 
ppr_nk dflags = O.showSDoc dflags . ppr_nk' where
  ppr_nk' (NKApp ag) = O.text "App @" O.<> (ppr $ _s_span ag)
  ppr_nk' (NKBind (BindLam sp)) = O.text "Lam @" O.<> ppr sp
  ppr_nk' (NKBind (BindNamed (Spand sp (Right n)))) = O.text "BindNamed " O.<> O.quotes (ppr n) <+> O.text "@" O.<> ppr sp
  ppr_nk' (NKBind (BindNamed (Spand sp (Left m)))) = O.text "Module " O.<> O.quotes (ppr m) <+> O.text "@" O.<> ppr sp

getShallowReferences :: HieAST a -> [LIdentifier]
getShallowReferences ast = map (Spand (nodeSpan ast)) $ M.keys $ nodeIdentifiers $ nodeInfo ast

is_var_name :: Identifier -> Bool
is_var_name (Left _) = True
is_var_name (Right n) = uncurry (&&) $ (>='a') &&& (<='z') $ head $ occNameString $ nameOccName n

maphead :: (a -> a) -> [a] -> [a]
maphead _ [] = []
maphead f (a:l) = (f a):l

maplast :: (a -> a) -> [a] -> [a]
maplast _ [] = []
maplast f (a:[]) = (f a):[]
maplast f (a:l) = a:(maplast f l)

loc_file :: Lens' RealSrcLoc FastString
loc_file = lens get' set' where
  get' l = srcLocFile l
  set' l f =
    mkRealSrcLoc f (srcLocLine l) (srcLocCol l)
    
span_file :: Lens' Span FastString
span_file = lens get' set' where
  get' sp = realSrcSpanStart sp ^. loc_file
  set' sp f =
    let l = realSrcSpanStart sp
        r = realSrcSpanEnd sp
    in mkRealSrcSpan (l & loc_file .~ f) (r & loc_file .~ f)

snip_src :: Span -> [String] -> [String]
snip_src sp ls =
  let (l, r) = (realSrcSpanStart sp, realSrcSpanEnd sp)
  in maplast (take (if srcLocLine l == srcLocLine r then (srcLocCol r - srcLocCol l) else srcLocCol r)) $ take (srcLocLine r - srcLocLine l + 1) $
     maphead (drop (srcLocCol l - 1)) $ drop (srcLocLine l - 1) ls

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = fmap (uncurry (:)) . splitOn' where
  splitOn' _ [] = mempty
  splitOn' a (b:l) =
    let (n, c) = splitOn' a l
    in if a == b then (mempty, n:c) else (b:n, c)

(!?~) :: Ord k => M.Map k a -> k -> Maybe (k, a)
m !?~ k | Just t@(k', a) <- M.lookupLE k m
        = if k' == k then Just t else Nothing
        | otherwise = Nothing