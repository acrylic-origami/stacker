{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, TemplateHaskell, LambdaCase, TupleSections, Rank2Types, MultiWayIf #-}
module Main where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import Text.Read ( readMaybe )
import qualified Data.Text.IO as TIO
import qualified GHC.Prof as Prof
import SrcLoc ( SrcLoc(..), SrcSpan, RealSrcSpan(..), mkSrcLoc, mkSrcSpan, containsSpan, mkRealSrcSpan, mkRealSrcLoc, realSrcSpanStart, realSrcSpanEnd )
import Name ( nameOccName, nameModule_maybe, Name(..) )
import GHC ( moduleNameString, moduleName, ModuleName(..) )
import OccName ( occNameString )
import Data.Bitraversable ( bisequence )
import Data.Bifunctor ( bimap )
import Data.Foldable ( fold, asum )
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
import Data.Maybe ( catMaybes, fromMaybe, fromJust, mapMaybe, maybeToList, listToMaybe )
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

import SysTools ( initSysTools )
import DynFlags ( DynFlags, defaultDynFlags )
import GHC.Paths (libdir)

import Lang
import Util

import Debug.Trace ( trace, traceShow )

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
      (txt2span_multi -> Just (fsrc, (al:ac:bl:bc:_))) ->
        Just $ mkRealSrcSpan (mkRealSrcLoc fsrc al ac) (mkRealSrcLoc fsrc bl (bc + 1)) -- fsLit $ T.unpack $ costCentreModule -- +1 because somehow the HIE loc and the prof file loc are different with their inclusive/exclusive
      (txt2span_single -> Just (fsrc, (l:ac:bc:_))) ->
        Just $ mkRealSrcSpan (mkRealSrcLoc fsrc l ac) (mkRealSrcLoc fsrc l (bc + 1)) -- fsLit $ T.unpack $ costCentreModule
      _ -> Nothing
    -- txt2span' :: T.Text -> Maybe (FastString, [Int])
    txt2span_multi = fmap (fsLit *** mapMaybe readMaybe) . ((uncons . snd) =<<) . uncons . map T.unpack . getAllTextSubmatches . (=~ ("^(.*):\\(([0-9]+),([0-9]+)\\)\\-\\(([0-9]+),([0-9]+)\\)$" :: T.Text)) -- safe drop 1
    txt2span_single = fmap (fsLit *** mapMaybe readMaybe) . ((uncons . snd) =<<) . uncons . map T.unpack . getAllTextSubmatches . (=~ ("^(.*):([0-9]+):([0-9]+)\\-([0-9]+)$" :: T.Text)) -- . uncurry (trace . show) . dupe

fNS :: [FastString]
fNS = ["Match"] -- I really hope the trees look the same for these two always every time: single level of first arg as 
aPPS :: [FastString]
aPPS = ["HsApp", "OpApp"]

has_node_type :: [FastString] -> HieAST a -> Bool
has_node_type constrs ast = or [ any ((ident==) . snd) $ S.toList $ nodeAnnotations $ nodeInfo ast | ident <- constrs ]

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
    let (fn_args, grhss) = first (M.keys) $ mconcat $ map arg_idents $ drop 1 $ nodeChildren ast
        fn_names = (M.keys $ generateReferencesMap $ [head $ nodeChildren ast])
        (next_names, next_store) = mconcat $ map (mk_pt_store' False) grhss
        fn_key = if has_node_constr ["HsLam"] ast then BindLam (nodeSpan ast) else BindNamed (nodeSpan ast) (head (if null fn_names then error (ppr_ast dflags ast) else fn_names))
    in flip const (ast, dflags, (grhss), (fn_names, fn_args), next_store, fn_key)
      $ (
          [] -- clear accumulated names
          , next_store
          & ps_binds %~ (
              flip (foldr (flip (M.insertWith (const . trace "Name is arg'd to two functions")) fn_key)) (S.toList $ S.fromList $ fn_args <> next_names) -- insert all fn_arg names pointing to fn_key. assume/invariant no collisions even with mulitple arg sets: given different names
            )
          & (ps_app_groups . ag_ident_map) %~ (flip (foldr (\(fn, n) m -> fromMaybe id (M.insert fn <$> (m M.!? n)) m)) (liftA2 (,) fn_names next_names))
          -- & ps_binds %~ (M.unionsWith (<>) . (:(map (fuzzy_patbinds) grhss))) -- search entire function body for patterns ONLY HERE, at the top of a function
        )
  mk_pt_store' within_app ast =
    let is_this_app = has_node_constr aPPS ast
        is_this_bind = has_node_constr ["BindStmt"] ast -- any (any (\case { PatternBind {} -> True; _ -> False }) $ S.toList $ identInfo) $ nodeIdentifiers $ nodeInfo ast
        is_this_case = has_node_constr ["HsCase"] ast
        -- next_within_app = not is_this_bind && (is_this_app || within_app)
        dig next_within_app = mconcat $ map (mk_pt_store' next_within_app) (nodeChildren ast)
    in -- trace (ppr_safe dflags $ map (\ast' -> (nodeIdentifiers $ nodeInfo ast', nodeAnnotations $ nodeInfo ast', is_this_bind)) (ast : nodeChildren ast)) $
       if | is_this_bind || is_this_case
          -> let (bindee_names, bindee_store) =
                    if is_this_bind
                      then mk_pt_store' False $ (!!1) $ nodeChildren ast
                      else mk_pt_store' False $ head $ nodeChildren ast
                 (next_names, next_store) =
                    if is_this_bind
                      then dig False
                      else mconcat $ map (mk_pt_store' False) binder_grhss
                 (binder_identmap, binder_grhss) =
                    if is_this_bind
                      then (generateReferencesMap $ take 1 $ nodeChildren ast, undefined)
                      else mconcat $ map arg_idents $ tail $ nodeChildren ast
                 bindee_app_idents = S.toList $ S.fromList $ concat $ M.elems $ bindee_store ^. ps_app_groups ^. ag_ident_map
                 binders = concatMap (uncurry (liftA2 (,)) . (pure *** map fst)) $ M.toList binder_identmap -- [(Span, Identifier)]
                 bindee_binders = liftA2 (,) bindee_app_idents binders
                 bindee_store' = bindee_store <> PtStore {
                    _ps_app_groups = AppGroups {
                      _ag_ident_map = M.fromList $ map (swap . (pure *** fst)) bindee_binders, -- a bit uncomfortable since I'm relying on none of the elements to change in the RHS of the map...
                      _ag_span_map = mempty -- but that's where the "app_idents" in `bindee_app_idents` is coming from, so just don't break it.
                    },
                    _ps_binds = M.fromList $ concatMap (uncurry (liftA2 (,)) . (s_payload *** pure . uncurry BindNamed . swap)) bindee_binders
                  }
            in flip const (dflags, ast, bindee_names, bindee_store, binder_identmap, binder_grhss, next_names, next_store) $ (
                bindee_names -- make binds be name boundaries
                , next_store <> bindee_store'
              )
            
          | has_node_type ["HsExpr"] ast
          -> let (next_names, next_store) = dig True
                 this_names = M.keys $ generateShallowReferencesMap [ast]
                 next_names' = next_names <> this_names
            in if not within_app
              then (
                  [] -- clear accumulated names
                  , next_store & ps_app_groups %~ (
                      (ag_span_map %~ (<> (SegFlat [(nodeSpan ast, Spand (nodeSpan ast) next_names')])))
                      . (ag_ident_map %~ (\m -> foldr (flip (M.insertWith (<>)) [Spand (nodeSpan ast) next_names']) m next_names')) -- up to 
                    )
                ) -- form appgroup and shove names into it
              else (next_names <> this_names, next_store)
              
          | otherwise -> dig within_app
          -- | otherwise
          -- -> let this_names = M.keys $ nodeIdentifiers $ nodeInfo ast -- only aggregate from the names that exist at this node
          --        next_store' = if not is_this_app
          --         then next_store & (ps_app_groups . ag_ident_map) %~ (\m -> foldr (flip M.insert this_names) m this_names) -- if this is a root-ish node 
          --         else next_store
          --   in ( this_names <> next_names, next_store' )

xselfmap :: (a -> a -> b) -> [a] -> [b]
xselfmap f (a:l) = (map (f a) l) <> xselfmap f l
xselfmap _ _ = []

pt_search :: DynFlags -> PtStore a -> Segs Prof.CostCentre -> Either BindKey Identifier -> ([Gr.Node], Gr (NodeKey a) ())
pt_search dflags (PtStore {..}) cs_segs = 
  let state_step :: [NodeKey a] -> (M.Map (NodeKey a) Gr.Node, Gr.Node) -> (M.Map (NodeKey a) Gr.Node, Gr.Node)
      state_step [] t = t
      state_step l (a, s) = ((M.union a . M.fromList) &&& snd . last) $ zip l [s..] -- (((a,) . M.fromList) &&& snd . last) $ zip l [s..]
      -- nodes :: M.Map (NodeKey a) Gr.Node
      nodes = -- (uncurry $ uncurry $ uncurry $ NodeStore . flip const)
        (`evalState` 0) $
          return mempty
          & mapState (state_step (map NKApp $ concat $ M.elems (_ps_app_groups ^. ag_ident_map)))
          & mapState (state_step (map NKBind $ M.elems _ps_binds))
        
      g0 = Gr.mkGraph (map swap $ M.toList nodes) mempty
      r0 = ([], g0)
      
      -- merge_decomps :: State (...) [([Gr.Node], Gr (NodeKey a) b)] -> State (...) ([Gr.Node], Gr (NodeKey a) b)
      -- basically a custom mconcat
      merge_decomps = fmap $ foldr1 (uncurry bimap . ((<>) *** flip (foldr Gr.insEdge) . Gr.labEdges)) . toNonEmpty r0  -- egregiously wasteful; think of a better way
      ident_in_cs ident = not $ ((null . segfind cs_segs) <$> (ident_span ident)) `elem` [Nothing, Just True]
      
      -- pt_search' :: Bool -> Identifier -> (Maybe Gr.Node, Gr (NodeKey a) ())
      pt_search' :: Either BindKey Identifier -> State (S.Set AppGroup) ([Gr.Node], Gr (NodeKey a) ())
      pt_search' m_ident =
        let ((next, this_nodes), within_cs) = case m_ident of
              Left bk | m_this_node <- nodes M.!? NKBind bk ->
                  case bk of
                    BindNamed _fn_span fn_ident -> (
                        (pt_search' (Right fn_ident), maybeToList m_this_node)
                        , ident_in_cs fn_ident
                      )
                    BindLam fn_span ->
                      let next_nodes' = concatMap s_payload $ segfind (_ps_app_groups ^. ag_span_map) fn_span
                      in (
                          (
                              merge_decomps $ mapM (pt_search' . Right) $ unique next_nodes'
                              , maybeToList m_this_node
                            )
                          , not $ null $ segfind cs_segs fn_span
                        )
              Right ident -> (
                  if | Just ags' <- (_ps_app_groups ^. ag_ident_map) M.!? ident
                     -- , Nothing <- (nodes !? (NKApp ag)) >>= (flip match gr) -- crap, can't build the graph from the bottom up because I need to anti-cycle, so children will have to do the matching
                     -> (
                        do
                          visited <- get
                          let ags = S.fromList ags' S.\\ visited
                              idents = unique $ concatMap s_payload $ S.toList ags
                          put $ S.union visited ags
                          merge_decomps $ mapM (pt_search' . Left . BindNamed undefined) idents
                        , M.elems $ M.restrictKeys nodes (S.fromList $ map NKApp ags') -- don't worry about the undefined: the Map doesn't pay attention to it for the lookup
                      )
                     
                     | otherwise -> (return r0, mempty)
                  , ident_in_cs ident
                )
        in if within_cs
          then bisequence (
              return this_nodes,
              fmap (\(next_nodes, next_gr) ->
                  foldr (\(l, r) -> Gr.insEdge (l, r, ())) next_gr (liftA2 (,) next_nodes this_nodes)
                ) next
            )
          else return r0
        
      -- pt_search False ident
      --   | 
      --   , Just this_node <- nodes M.!? fnkey
      --   = let (next_decomps, next_gr) = 
      --     in (this_node, foldr (\decomp -> Gr.insEdge (this_node, node' $ fst decomp, ())) next_gr (catMaybes next_decomps))
        
      --   | Just bind_pt <- _ps_binds M.!? ident
      --   =
          
  in (`evalState` mempty) . pt_search'

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
  (s_targ:fprof:dhies) <- getArgs
  fhies <- fmap concat $ mapM getHieFilesIn $ dhies
  (hies, _) <- foldrM (\f (l, nc') -> (first ((:l) . hie_file_result)) <$> readHieFile nc' f) ([], nc) fhies
  -- print fhies
  -- putStrLn $ unlines $ map (ppr_ast dflags) $ M.elems $ getAsts $ hie_asts $ head hies
  
  mprof <- Prof.decode' <$> TIO.readFile fprof
  case mprof of
    Left s -> error s
    Right (Prof.costCentres -> Just cs_tree) -> -- curry $ fmap (uncurry Tr.Node) . bisequence .
      let loc_cs_forest = Tr.foldTree (
              curry $
              uncurry (flip fromMaybe)
              . ((uncurry (liftA2 (fmap (\x -> [x]) . Tr.Node)) . second pure) &&& snd)
              . (bisequence . (pure &&& cs_span) *** concat)
            ) cs_tree -- (SrcSpan, Profile)
          pts = map (mk_pt_store dflags) (concatMap (M.elems . getAsts . hie_asts) hies)
          -- note: also able to map directly to Binds and BindKeys as well, since the PtStore map type right-hands can be coalesced to NodeKeys
          -- bound'' :: STree.STree [STInterval.Interval (Locd ())] (Locd ())
          -- bound'' = STree.fromList $ concatMap (map (seg2intervalish . (fnkey_span &&& const ())) . M.elems . (^. ps_fns)) pts
          bound' = concatMap (map (seg2intervalish . (fnkey_span &&& id)) . M.elems . (^. ps_binds)) pts
          bound = STree.fromList bound'
          
          ident_str_lookup = M.fromList . mapMaybe (\case
              BindLam _ -> Nothing
              BindNamed s i -> Just $ (
                case i of 
                  Left mn -> (moduleNameString mn, "")
                  Right n -> (fromMaybe "" $ moduleNameString . moduleName <$> nameModule_maybe n, occNameString $ nameOccName n)
                , s)
            )
          -- bound''' :: M.Map (String, String) [Span]
          -- bound''' = M.unionsWith (<>) (
          --     -- map (M.map pure . ident_str_lookup . M.keys . (^. ps_binds)) pts -- i don't actually think that binds are referenced in the cost center stack trace
          --     -- ++
          --     map (
          --         M.map pure
          --         . ident_str_lookup
          --         . M.elems
          --         . (^. ps_fns)
          --       ) pts
          --   )
          i_targ = read s_targ :: Int
          targ_fold css@(cs, _) l = 
            let next = if Prof.costCentreNo cs == i_targ then Just css else Nothing
                l' = mconcat $ map (First . bisequence . second pure) l
            in case getFirst l' of -- pass up entire subtree if we haven't found the module yet, otherwise constrain to single branch
              Just (cs_matched, t) -> (Just cs_matched, Tr.Node css [t])
              Nothing -> (next, Tr.Node css (map snd l))
          (targ, targ_t) = unzip $ mapMaybe (bisequence . second pure . Tr.foldTree targ_fold) loc_cs_forest
          -- targ_segs' = SegTree $ STree.fromList $ concatMap (Tr.foldTree ((.concat) . (:) . seg2intervalish . second (const ()) . swap)) targ_t
          targ_segs = SegTree $ STree.fromList $ concatMap (Tr.foldTree ((.concat) . (:) . seg2intervalish . swap)) targ_t
          bks = ivl_payloads $ concatMap (STree.superintervalQuery bound . span2interval . snd) targ
          grs = [
              pt_search dflags pt targ_segs (Left bk)
              | bk <- bks
              , pt <- pts
            ]
      in flip const (targ, bound, dflags, loc_cs_forest, pts, grs) $ do -- $ trace (show $ length loc_cs_forest)
        pure ()
        -- putStrLn $ ppr_safe dflags $ M.elems . (^. ps_fns) <$> pts
        -- putStrLn $ show $ [(b, sp, b == sp) | (_, sp) <- targ, b <- (bound''' M.! ("Main", "has_node_constr"))]
        print $ concatMap (\(_, s) -> ivl_locs $ STree.superintervalQuery bound $ span2interval s) targ
        -- print bound''
        -- putStrLn $ ppr_safe dflags bound'
        print (length bks, map ((length . (_ag_ident_map . _ps_app_groups) &&& length . _ps_binds)) pts)
        putStrLn $ Tr.drawForest $ fmap (show . snd) <$> targ_t
        const (pure ()) $ ppr_ dflags $ map (S.fromList . concat . M.elems . _ag_ident_map . _ps_app_groups) pts
        -- -- putStrLn $ ppr_safe dflags bound -- <$> (bound M.!? targ)
        const (pure ()) $ print $ sum $ map (length . filter id . xselfmap (curry $ not . uncurry (||) . (uncurry (==) &&& (S.null . uncurry (S.\\) . both (S.fromList . s_payload)))) . concat . M.elems . (^. (ps_app_groups . ag_ident_map))) pts
        putStrLn $ unlines $ map (\(n, gr) ->
            unlines $ map (uncurry ((++) . (++"->")) . both (fromMaybe "" . fmap (ppr_nk dflags) . Gr.lab gr)) $ S.toList $ S.fromList $ Gr.edges gr
          ) grs
        {- $ map (\case -- gr_prettify (ppr_safe dflags) id
            (Just node, gr) -> gr_prettify (ppr_safe dflags) id gr
            (Nothing, _) -> mempty
          ) grs -}
        