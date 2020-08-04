{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, TemplateHaskell, LambdaCase, TupleSections, Rank2Types, MultiWayIf #-}
module Main where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Prof as Prof
import SrcLoc ( SrcLoc(..), SrcSpan, RealSrcSpan(..), mkSrcLoc, mkSrcSpan, containsSpan, mkRealSrcSpan, mkRealSrcLoc, realSrcSpanStart, realSrcSpanEnd )
import Name ( nameOccName, Name(..) )
import GHC ( moduleNameString, ModuleName(..) )
import OccName ( occNameString )
import Data.Bitraversable ( bisequence )
import Data.Bifunctor ( bimap )
import Data.Foldable ( fold )
import qualified Data.Tree as Tr
import FastString ( FastString(..), fsLit )
import NameCache ( NameCache(..), initNameCache )
import Data.Text.IO ( readFile )
import System.Environment ( getArgs )
import HieBin
import HieTypes
import HieUtils
import Text.Regex.TDFA
import Text.Regex.Base.RegexLike ( AllTextSubmatches(..) )
import Data.Monoid ( First(..) )
import Data.Maybe ( catMaybes, fromMaybe, fromJust, mapMaybe, maybeToList )
import Data.List ( uncons, partition )
import Data.Foldable ( foldrM, foldlM )
import Control.Arrow ( (***), (&&&), first, second )
import Data.Semigroup ( Last(..) )
import Control.Applicative ( liftA2 )
import UniqSupply ( mkSplitUniqSupply )
import Control.Lens ( makeLenses )
import Control.Monad.State ( MonadState(..), State(..), StateT(..), evalState, mapState )
import qualified Control.Lens as L
import Control.Lens.Operators

import qualified Data.SegmentTree as STree
import qualified Data.SegmentTree.Interval as STInterval
import Data.SegmentTree.Measured

import qualified Data.Graph.Inductive as Gr
import Data.Graph.Inductive.PatriciaTree ( Gr(..) )

import System.Directory
import System.FilePath

import Outputable ( showSDoc, interppSP, Outputable(..) )

import SysTools ( initSysTools )
import DynFlags ( DynFlags, defaultDynFlags )
import GHC.Paths (libdir)

import Lang
import Util

import Debug.Trace ( trace )

ppr_safe :: Outputable a => DynFlags -> a -> String
ppr_safe d = showSDoc d . interppSP . pure

ppr_ :: Outputable a => DynFlags -> a -> IO ()
ppr_ d = putStrLn . ppr_safe d

dynFlagsForPrinting :: IO DynFlags
dynFlagsForPrinting = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings ([], [])
  
makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []

-- NOTE: `getHieFilesIn` originates from HieDb - see ./LICENSE_hiedb
-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path = do
  exists <- doesPathExist path
  if exists then do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    if isFile && ("hie" `isExtensionOf` path) then do
      path' <- canonicalizePath path
      return [path']
    else if isDir then do
      cnts <- listDirectory path
      withCurrentDirectory path $ foldMap getHieFilesIn cnts
    else return []
  else
    return []

ppr_ast_node :: DynFlags -> HieAST a -> String
ppr_ast_node d = ppr_safe d . ((nodeAnnotations &&& M.map (identInfo) . nodeIdentifiers) . nodeInfo &&& nodeSpan)

ppr_ast :: DynFlags -> HieAST a -> String
ppr_ast d = uncurry ((++) . (++"\n")) . (ppr_ast_node d &&& unlines . map (ppr_ast d) . nodeChildren)

--------------------------------------------------------------

cs_span :: Prof.CostCentre -> Maybe RealSrcSpan
cs_span (Prof.CostCentre {..}) = txt2span =<< costCentreSrc
  where
    txt2span sloc = case sloc of
      (T.unpack -> '<':_) -> Nothing -- one of the <built-in>, <entire-module>, etc.
      (txt2span' -> Just (fsrc, (al:ac:bl:bc:_))) ->
        Just $ mkRealSrcSpan (mkRealSrcLoc fsrc al ac) (mkRealSrcLoc fsrc bl bc) -- fsLit $ T.unpack $ costCentreModule
      _ -> Nothing
    -- txt2span' :: T.Text -> Maybe (FastString, [Int])
    txt2span' = fmap (fsLit *** map read) . uncons . map T.unpack . getAllTextSubmatches . (=~ ("^(.*?):\\((\\d+),(\\d+)\\)-\\((\\d+),(\\d+)\\)$" :: T.Text))

fNS :: [FastString]
fNS = ["HsLam", "AbsBinds"] -- I really hope the trees look the same for these two always every time: single level of first arg as 
aPPS :: [FastString]
aPPS = ["HsApp", "OpApp"]

has_node_constr :: [FastString] -> HieAST a -> Bool
has_node_constr constrs ast = or $ catMaybes [ (fmap ((==ident) . fst) $ S.lookupGT (ident, "") $ nodeAnnotations $ nodeInfo ast) | ident <- constrs ]

shallower :: (HieAST a -> Bool) -> HieAST a -> ([HieAST a], [HieAST a])
shallower f a | f a = ([], [a])
              | otherwise = first (a:) $ mconcat $ map (shallower f) (nodeChildren a)

fn_ident :: IdentMap a -> [Identifier]
fn_ident = M.keys . M.filter (any (any (\case { MatchBind -> True; _ -> False }) . S.toList . identInfo . snd))

arg_idents :: HieAST a -> (IdentMap a, [HieAST a])
arg_idents = first generateShallowReferencesMap . shallower (has_node_constr ["GRHS"])

mk_pt_store :: DynFlags -> HieAST TypeIndex -> PtStore TypeIndex
mk_pt_store dflags = ((ps_app_groups . ag_span_map) %~ (\(SegFlat s) -> SegTree $ STree.fromList $ map seg2intervalish s)) . snd . mk_pt_store' False where
  mk_pt_store' :: Bool -> HieAST TypeIndex -> ([Identifier], PtStore TypeIndex)
  mk_pt_store' _ ast | has_node_constr fNS ast =
    let (idents, grhss) = arg_idents ast
        is_matchbind = any (\case { MatchBind -> True; _ -> False }) . S.toList . identInfo
        (fn_names, fn_args) = both (map fst) $ partition (any is_matchbind . map snd . snd) (M.toList idents)
        (_, next_store) = mconcat $ map (mk_pt_store' False) grhss
        fn_key = if has_node_constr ["HsLam"] ast then FnLam (nodeSpan ast) else FnNamed (head fn_names)
    in (
          [] -- clear accumulated names
          , next_store
          & ps_fns %~ (
              flip (foldr (flip (M.insertWithKey (error "Name is arg'd to two functions")) fn_key)) fn_args -- insert all fn_arg names pointing to fn_key. assume/invariant no collisions even with mulitple arg sets: given different names
            )
          -- & ps_binds %~ (M.unionsWith (<>) . (:(map (fuzzy_patbinds) grhss))) -- search entire function body for patterns ONLY HERE, at the top of a function
        )
  mk_pt_store' within_app ast =
    let is_this_app = has_node_constr aPPS ast
        is_this_bind = has_node_constr ["BindStmt"] ast -- any (any (\case { PatternBind {} -> True; _ -> False }) $ S.toList $ identInfo) $ nodeIdentifiers $ nodeInfo ast
        is_this_case = has_node_constr ["HsCase"] ast
        -- next_within_app = not is_this_bind && (is_this_app || within_app)
        dig next_within_app = mconcat $ map (mk_pt_store' next_within_app) (nodeChildren ast)
    in if | is_this_bind
          -> let (bindee_names, bindee_store) = mk_pt_store' False $ (!!1) $ nodeChildren ast
                 bg = BindGroup (nodeSpan ast) bindee_store
                 (next_names, next_store) = dig False
                 binder_idents = M.keys $ generateReferencesMap $ take 1 $ nodeChildren ast -- still strange that generateReferencesMap takes a foldable
            in ( -- push all new bindings
                bindee_names -- make binds be name boundaries
                , next_store & (ps_binds %~ (\m -> foldr (flip M.insert bg) m binder_idents))
              )
          | is_this_case
          -> let (bindee_names, bindee_store) = mk_pt_store' False $ head $ nodeChildren ast
                 bg = BindGroup (nodeSpan ast) bindee_store
                 (binder_identmap, binder_grhss) = mconcat $ map arg_idents $ tail $ nodeChildren ast
                 (next_names, next_store) = mconcat $ map (mk_pt_store' False) binder_grhss
            in (
                bindee_names -- make binds be name boundaries
                , next_store & (ps_binds %~ (\m -> foldr (flip M.insert bg) m (M.keys binder_identmap)))
              )
          | otherwise
          -> let (next_names, next_store) = dig True
                 this_names = M.keys $ generateShallowReferencesMap [ast]
                 next_names' = next_names <> this_names
            in if not within_app
              then (
                  [] -- clear accumulated names
                  , next_store & ps_app_groups %~ (
                      (ag_span_map %~ (mappend (SegFlat [(nodeSpan ast, next_names')])))
                      . (ag_ident_map %~ (\m -> foldr (flip M.insert next_names') m next_names')) -- up to 
                    )
                ) -- form appgroup and shove names into it
              else (next_names <> this_names, next_store)
          -- | otherwise
          -- -> let this_names = M.keys $ nodeIdentifiers $ nodeInfo ast -- only aggregate from the names that exist at this node
          --        next_store' = if not is_this_app
          --         then next_store & (ps_app_groups . ag_ident_map) %~ (\m -> foldr (flip M.insert this_names) m this_names) -- if this is a root-ish node 
          --         else next_store
          --   in ( this_names <> next_names, next_store' )

pt_search :: DynFlags -> PtStore a -> Segs (Maybe Prof.CostCentre) -> Bool -> Identifier -> (Maybe Gr.Node, Gr (NodeKey a) ())
pt_search dflags (PtStore {..}) cs_segs =
  let state_step :: [NodeKey a] -> (M.Map (NodeKey a) Gr.Node, Gr.Node) -> (M.Map (NodeKey a) Gr.Node, Gr.Node)
      state_step l (a, s) = ((M.union a . M.fromList) &&& snd . last) $ zip l [s..] -- (((a,) . M.fromList) &&& snd . last) $ zip l [s..]
      -- nodes :: M.Map (NodeKey a) Gr.Node
      nodes = (`evalState` 0) $
          return mempty
          & mapState (state_step (map NKApp $ M.elems (_ps_app_groups ^. ag_ident_map)))
          & mapState (state_step (map NKBind $ M.elems (_ps_binds)))
          & mapState (state_step (map NKFn $ M.elems (_ps_fns)))
        
      g0 = Gr.mkGraph (map swap $ M.toList nodes) mempty
      
      -- merge_decomps :: [(Maybe Gr.Node, Gr (NodeKey a) b)] -> ([Maybe Gr.Node], Gr (NodeKey a) b)
      merge_decomps = foldr1 (uncurry bimap . ((<>) *** flip (foldr Gr.insEdge) . Gr.labEdges)) . fmap (first pure) . toNonEmpty (Nothing, g0) -- egregiously wasteful; think of a better way
      
      -- pt_search' :: Bool -> Identifier -> (Maybe Gr.Node, Gr (NodeKey a) ())
      pt_search' visited is_fn ident =
        let ((next_nodes, next_gr), this_node)
              | is_fn
              , Just ag <- (_ps_app_groups ^. ag_ident_map) M.!? ident
              , not $ ag `S.member` visited
              -- , Nothing <- (nodes !? (NKApp ag)) >>= (flip match gr) -- crap, can't build the graph from the bottom up because I need to anti-cycle, so children will have to do the matching
              = (merge_decomps $ map (pt_search' (S.insert ag visited) False) ag, nodes M.!? NKFn (FnNamed ident))
              
              | Just fnkey <- _ps_fns M.!? ident
              , m_this_node <- nodes M.!? (NKFn fnkey)
              = case fnkey of
                FnNamed fn_ident -> (first pure $ pt_search' visited True fn_ident, m_this_node)
                FnLam fn_span ->
                  let next_nodes' = concat $ segfind (_ps_app_groups ^. ag_span_map) fn_span
                  in (merge_decomps $ map (pt_search' visited False) next_nodes', m_this_node)
                  
              | Just bg@(BindGroup _sp pts) <- _ps_binds M.!? ident
              , m_this_node <- nodes M.!? (NKBind bg)
              = (merge_decomps $ map (pt_search' visited False) (concat $ M.elems $ pts ^. ps_app_groups ^. ag_ident_map), m_this_node) -- TODO figure out what bindees are actually pointing to: for now, only bind directly to the shallow app-names          
              
              | otherwise = ((mempty, g0), Nothing)
          in snd (dflags, (this_node, foldr (\node -> fromMaybe id $ Gr.insEdge <$> ((, node, ()) <$> this_node)) next_gr (catMaybes next_nodes)))
      
      -- pt_search False ident
      --   | 
      --   , Just this_node <- nodes M.!? fnkey
      --   = let (next_decomps, next_gr) = 
      --     in (this_node, foldr (\decomp -> Gr.insEdge (this_node, node' $ fst decomp, ())) next_gr (catMaybes next_decomps))
        
      --   | Just bind_pt <- _ps_binds M.!? ident
      --   =
          
  in pt_search' mempty

-- grhs_args :: HieAST a -> Maybe (IdentMap a)
-- grhs_args ast | has_node_constr ["GRHS"] ast = Just $ generateReferencesMap [ast]
--               | otherwise = getFirst $ foldMap (First . grhs_args) (nodeChildren ast)

{-
1. create list of all functions, nesting as necessary.
2. 
-}

main :: IO ()
main = do
  dflags <- dynFlagsForPrinting
  nc <- makeNc
  (targ:fprof:dhies) <- getArgs
  fhies <- fmap concat $ mapM getHieFilesIn $ dhies
  (hies, _) <- foldrM (\f (l, nc') -> (first ((:l) . hie_file_result)) <$> readHieFile nc' f) ([], nc) fhies
  
  -- putStrLn $ unlines $ map (ppr_ast dflags) $ M.elems $ getAsts $ hie_asts $ head hies
  
  mprof <- Prof.decode' <$> TIO.readFile fprof
  case mprof of
    Left s -> error s
    Right (Prof.costCentres -> Just cs_tree) ->
      let (Just loc_cs_tree) = Tr.foldTree (curry $ fmap (uncurry Tr.Node) . bisequence . (bisequence . (pure &&& cs_span) *** pure . catMaybes)) cs_tree -- (SrcSpan, Profile)
          pts = map (mk_pt_store dflags) (concatMap (M.elems . getAsts . hie_asts) hies)
          ident_str_lookup = M.fromList . map ((\case { Left mn -> moduleNameString mn; Right n -> occNameString $ nameOccName n }) &&& id)
          -- note: also able to map directly to Binds and FnKeys as well, since the PtStore map type right-hands can be coalesced to NodeKeys
          bound = M.unionsWith (<>) (
              map (M.map pure . ident_str_lookup . map fst . M.toList . (^. ps_binds)) pts
              ++ map (
                  M.map pure
                  . uncurry (M.unionWith (<>))
                  . (
                      ident_str_lookup
                      *** ident_str_lookup . mapMaybe (\case { FnNamed i -> Just i; FnLam _ -> Nothing })
                    )
                  . unzip
                  . M.toList
                  . (^. ps_fns)
                ) pts
            )
          grs = [
              pt_search dflags pt undefined False ident
              | idents <- maybeToList $ (bound M.!? targ)
              , ident <- idents
              , pt <- pts
            ]
      in do
        putStrLn $ unlines $ const (map (gr_prettify (ppr_safe dflags) id . snd) grs) $ map (\case 
            (Just node, gr) -> gr_prettify (ppr_safe dflags) id gr
            (Nothing, _) -> mempty
          ) grs
        