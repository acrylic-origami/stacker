{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, TemplateHaskell, LambdaCase, TupleSections, Rank2Types, MultiWayIf, DeriveGeneric #-}
module Main where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UB
import qualified Data.ByteString.Lazy.UTF8 as UBL
import qualified Data.Text.IO as TIO

-- GHC
import SrcLoc ( SrcLoc(..), SrcSpan, RealSrcSpan(..), mkSrcLoc, mkSrcSpan, containsSpan, mkRealSrcSpan, mkRealSrcLoc, realSrcSpanStart, realSrcSpanEnd )
import Name ( nameOccName, nameModule_maybe, Name(..) )
import GHC ( moduleNameString, moduleName, ModuleName(..) )
import OccName ( occNameString )
import FastString ( FastString(..), fsLit, unpackFS )
import Data.Text.IO ( readFile )
import Text.Read ( readMaybe )
import SysTools ( initSysTools )
import DynFlags ( DynFlags, defaultDynFlags )
import GHC.Paths (libdir)

-- data/control
import Data.Bitraversable ( bisequence )
import Data.Bifunctor ( bimap )
import Data.Foldable ( fold, asum )
import qualified Data.Tree as Tr
import Data.Semigroup ( First(..) )
import Data.Maybe ( catMaybes, fromMaybe, fromJust, mapMaybe, maybeToList, listToMaybe )
import Data.List ( uncons, partition, intersperse )
import Data.Foldable ( foldrM, foldlM )
import Control.Arrow ( (***), (&&&), first, second )
import Data.Semigroup ( Last(..) )
import Control.Applicative ( liftA2 )
import Control.Lens ( makeLenses )
import Control.Monad.State ( MonadState(..), State(..), StateT(..), evalState, mapState )
import qualified Control.Lens as L
import Control.Lens.Operators

-- prof & HIE
import qualified GHC.Prof as Prof
import HieBin
import HieTypes
import HieUtils

-- regex
import Text.Regex.TDFA
import Text.Regex.Base.RegexLike ( AllTextSubmatches(..) )

-- aeson
import Data.Aeson ( FromJSON(..), ToJSON(..) )
import qualified Data.Aeson as Aeson

-- data structures
import qualified Data.SegmentTree as STree
import qualified Data.SegmentTree.Interval as STInterval
import Data.SegmentTree.Measured

import qualified Data.Graph.Inductive as Gr
import Data.Graph.Inductive.PatriciaTree ( Gr(..) )

-- system
import System.Directory
import System.FilePath
import System.Environment ( getArgs )

-- local
import Stacker.Lang
import Stacker.Util

-- debug
import Debug.Trace ( trace, traceShow )

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
      fsize <- getFileSize path'
      if fsize == 0 then mempty else return [path']
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

-- fn_ident :: IdentMap a -> [Identifier]
-- fn_ident = M.keys . M.filter (any (any (\case { MatchBind -> True; _ -> False }) . S.toList . identInfo . snd)) 

lift_ctx_info :: (ContextInfo -> Bool) -> (HieAST a -> Bool)
lift_ctx_info f = (any (any f . S.toList . identInfo)) . nodeIdentifiers . nodeInfo

arg_idents :: HieAST a -> (([LIdentifier], [LIdentifier]), [HieAST a])
arg_idents = first (both (concatMap getShallowReferences) . partition (lift_ctx_info $ \case { MatchBind -> True; _ -> False })) . shallower (has_node_constr ["GRHS"])

mk_pt_store :: DynFlags -> HieAST TypeIndex -> PtStore TypeIndex
mk_pt_store dflags = ((ps_app_groups . ag_span_map) %~ (\(SegFlat s) -> SegTree $ STree.fromList $ map seg2intervalish s)) . snd . mk_pt_store' False . ([],) where
  mk_pt_store' :: Bool -> ([HieAST TypeIndex], HieAST TypeIndex) -> ([AppGroup], PtStore TypeIndex)
  mk_pt_store' _ (ast_stack, ast)
    | has_node_constr fNS ast
    , Just (First True) <- mconcat $ map (\ast' -> if has_node_constr ["GRHS"] ast' then Just (First False) else if has_node_constr ["FunBind"] ast' then Just (First True) else Nothing) (ast:ast_stack)
    , (((fn_name:_), fn_args), grhss@(_:_)) <- arg_idents ast
    = let next_ast_stack = (ast : ast_stack)
          -- fn_names = (M.keys $ generateReferencesMap $ [(\x -> if null x then error "1" else head x) $ nodeChildren ast])
          (next_names, next_store) = mconcat $ map (mk_pt_store' False . (next_ast_stack,)) grhss
          fn_key = if has_node_constr ["HsLam"] ast then BindLam (nodeSpan ast) else BindNamed fn_name
      in flip const (ast, dflags, (grhss), (fn_name, fn_args), next_store, fn_key)
        $ (
            [] -- clear accumulated names
            , next_store
            & ps_binds %~ (
                (bg_arg_bnd_map %~ (
                    flip (foldr (flip (M.insertWithKey (\n a b -> trace (ppr_safe dflags n <> " is arg'd to two functions: " <> ppr_safe dflags a <> " & " <> ppr_safe dflags b) a)) fn_key)) (filter (is_var_name . s_payload) $ unique fn_args)
                  ))
                . (
                    case fn_key of 
                      BindNamed i ->
                        (bg_bnd_app_map %~ (M.insertWith (<>) i next_names)) -- (bg_named_lookup %~ S.insert i) .
                      _ -> id
                  )
                 -- insert all fn_arg names pointing to fn_key. assume/invariant no collisions even with mulitple arg sets: given different names
              )
            -- & ps_binds %~ (M.unionsWith (<>) . (:(map (fuzzy_patbinds) grhss))) -- search entire function body for patterns ONLY HERE, at the top of a function
          )
  mk_pt_store' within_app (ast_stack, ast) =
    let is_this_app = has_node_constr aPPS ast
        is_this_bind = has_node_constr ["BindStmt"] ast -- any (any (\case { PatternBind {} -> True; _ -> False }) $ S.toList $ identInfo) $ nodeIdentifiers $ nodeInfo ast
        is_this_case = has_node_constr ["HsCase"] ast
        next_ast_stack = (ast : ast_stack)
        -- next_within_app = not is_this_bind && (is_this_app || within_app)
        dig next_within_app = mconcat $ map (mk_pt_store' next_within_app . (next_ast_stack,)) (nodeChildren ast)
    in -- trace (ppr_safe dflags $ map (\ast' -> (nodeIdentifiers $ nodeInfo ast', nodeAnnotations $ nodeInfo ast', is_this_bind)) (ast : nodeChildren ast)) $
       if | is_this_bind || is_this_case
          -> let (binder_ags, binder_store) =
                    if is_this_bind
                      then mk_pt_store' False (next_ast_stack, (!!1) $ nodeChildren ast)
                      else fromMaybe mempty $ mk_pt_store' False . (next_ast_stack,) <$> listToMaybe (nodeChildren ast) -- watching out for EmptyCase
                 bindees :: [LIdentifier]
                 (bindees, (next_names, next_store)) =
                    first (filter (is_var_name . s_payload)) $
                    if is_this_bind
                      then (
                          map (uncurry (flip Spand)) $ concatMap (bisequence . (pure *** map fst)) $ M.toList $ generateReferencesMap $ take 1 $ nodeChildren ast
                          , mempty
                        )
                      else (snd *** mconcat . map (mk_pt_store' False . (next_ast_stack,))) $ mconcat $ map arg_idents $ drop 1 $ nodeChildren ast
                 -- bindees = concatMap (uncurry (liftA2 (,)) . (pure *** map fst)) $ M.toList bindee_identmap -- [(Span, Identifier)]
                 binder_store' = binder_store & (ps_binds %~ mappend BindGroups {
                    -- _bg_named_lookup = S.fromList bindees,
                    _bg_arg_bnd_map = mempty,
                    _bg_bnd_app_map = M.fromList $ map (,binder_ags) bindees
                  })
            in flip const (dflags, ast, binder_ags, binder_store) $ (
                binder_ags <> next_names -- make binds be name boundaries
                , binder_store' <> next_store
              )
            
          | has_node_type ["HsExpr"] ast
          -> let (next_ags, next_store) = dig True
                 this_names = getShallowReferences ast
                 next_names' = concatMap s_payload next_ags <> this_names -- throw out locs of direct descendants: they're part of _this_ app group now
                 next_ag = Spand (nodeSpan ast) next_names'
            in (
                [next_ag] -- pass this up to a bind to make deps, invariant that it doesn't hit another app thing (see the top-level `otherwise` clause)
                , if not within_app
                  then next_store & ps_app_groups %~ (
                      (ag_span_map %~ mappend (SegFlat [Spand (nodeSpan ast) next_ag]))
                      . (ag_ident_map %~ (\m -> foldr (flip (M.insertWith (<>)) [next_ag]) m next_names')) -- up to 
                    )
                  else next_store
              ) -- form appgroup and shove names into it
              
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

type HollowNodeGr = Gr (NodeKeyCtor, Span) Span
type NodeGr a = Gr (NodeKey a) LIdentifier
type NodeState a = State (S.Set AppGroup) ([Gr.Node], [Gr.LEdge LIdentifier])

pt_search :: DynFlags -> PtStore a -> Segs Prof.CostCentre -> Either BindKey LIdentifier -> ([Gr.Node], NodeGr a)
pt_search dflags (PtStore {..}) cs_segs = 
  let state_step :: [NodeKey a] -> (M.Map (NodeKey a) Gr.Node, Gr.Node) -> (M.Map (NodeKey a) Gr.Node, Gr.Node)
      state_step [] t = t
      state_step l (a, s) = ((M.union a . M.fromList) &&& snd . last) $ zip l [s..] -- (((a,) . M.fromList) &&& snd . last) $ zip l [s..]
      -- nodes :: M.Map (NodeKey a) Gr.Node
      nodes = -- (uncurry $ uncurry $ uncurry $ NodeStore . flip const)
        (`evalState` 0) $
          return mempty
          & mapState (state_step (map NKApp $ concat $ M.elems (_ps_app_groups ^. ag_ident_map)))
          & mapState (state_step (map (NKBind . BindNamed) $ M.keys $ _ps_binds ^. bg_bnd_app_map))
        
      r0 = ([], [])
      
      -- merge_decomps :: State (...) [([Gr.Node], Gr (NodeKey a) b)] -> State (...) ([Gr.Node], Gr (NodeKey a) b)
      -- basically a custom mconcat
      -- merge_decomps = fmap $ foldr (uncurry bimap . ((<>) *** flip (foldr Gr.insEdge) . Gr.labEdges)) r0  -- egregiously wasteful; think of a better way
      ident_in_cs ident = not $ null $ segfind cs_segs $ s_span ident
      
      with_ags :: ([LIdentifier] -> NodeState a) -> [AppGroup] -> NodeState a
      with_ags f ags = do
        visited <- get
        let ags' = S.fromList ags S.\\ visited
            idents = unique $ concatMap s_payload $ S.toList ags'
        put $ S.union visited ags'
        f idents
      
      with_ag :: (LIdentifier -> NodeState a) -> AppGroup -> NodeState a
      with_ag f ag = do
        visited <- get
        if ag `S.member` visited
          then return mempty
          else do
            put $ S.insert ag visited
            mconcat <$> mapM f (s_payload ag)
      
      -- pt_search' :: Bool -> LIdentifier -> (Maybe Gr.Node, Gr (NodeKey a) ())
      pt_search' :: Either BindKey LIdentifier -> NodeState a
      pt_search' m_ident =
        let (node_sucs, within_cs) = case m_ident of -- trace (ppr_safe dflags m_ident) $ 
              Left bk ->
                let (edge_ags, within_cs) = case bk of
                      BindNamed fn_ident -> (
                          (maybeToList $ (_ps_app_groups ^. ag_ident_map) !?~ fn_ident)
                          <> (maybeToList $ (_ps_binds ^. bg_bnd_app_map) !?~ fn_ident)
                          , ident_in_cs fn_ident
                        )
                      BindLam fn_span -> (
                          map ((Spand fn_span undefined,) . pure) (segfind (_ps_app_groups ^. ag_span_map) fn_span)
                          , not $ null $ segfind cs_segs fn_span
                        )
                in (
                    [
                      ((node, edge), with_ag (pt_search' . Right) ag)
                      | (edge, ags) <- edge_ags
                      , ag <- ags
                      , node <- maybeToList $ nodes M.!? (NKApp ag)
                    ]
                      -- $ M.toList $ M.restrictKeys nodes (S.fromList $ map NKApp )
                    , within_cs
                  )
              Right ident -> (, ident_in_cs ident) $ 
                  if 
                     | Just (bnd, ags) <- (_ps_binds ^. bg_bnd_app_map) !?~ ident
                     -> (
                        mapMaybe (bisequence . (
                            fmap (,bnd) . (nodes M.!?) . NKApp
                            &&& Just . with_ag (pt_search' . Right)
                          )) ags
                      )
                     | Just (arg, bk) <- (_ps_binds ^. bg_arg_bnd_map) !?~ ident
                     -- , Nothing <- (nodes !? (NKApp ag)) >>= (flip match gr) -- crap, can't build the graph from the bottom up because I need to anti-cycle, so children will have to do the matching
                     -> (
                        maybeToList $ bisequence (
                            fmap (,arg) $ nodes M.!? (NKBind bk),
                            Just $ pt_search' (Left bk)
                          )
                      )
                     
                     | otherwise -> mempty
        in if within_cs
          then bisequence (
              return (map (fst . fst) node_sucs),
              foldrM (\((this_node, this_edge_lab), next) next_edges -> do
                  (next_nodes, next_edges') <- next
                  return $ map (this_node, , this_edge_lab) next_nodes <> next_edges <> next_edges'
                ) mempty node_sucs
            )
          else return r0
        
      -- pt_search False ident
      --   | 
      --   , Just this_node <- nodes M.!? fnkey
      --   = let (next_decomps, next_gr) = 
      --     in (this_node, foldr (\decomp -> Gr.insEdge (this_node, node' $ fst decomp, ())) next_gr (catMaybes next_decomps))
        
      --   | Just bind_pt <- _ps_binds M.!? ident
      --   =
          
  in second (Gr.mkGraph (map swap $ M.toList nodes)) . (`evalState` mempty) . pt_search'

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
          lined_srcs = M.fromList $ map (hie_hs_file &&& lines . UB.toString . hie_hs_src) hies
          -- note: also able to map directly to Binds and BindKeys as well, since the PtStore map type right-hands can be coalesced to NodeKeys
          -- bound'' :: STree.STree [STInterval.Interval (Locd ())] (Locd ())
          -- bound'' = STree.fromList $ concatMap (map (seg2intervalish . (bindkey_span &&& const ())) . M.elems . (^. ps_fns)) pts
          
          bound_fns' = concatMap (map seg2intervalish . map (uncurry Spand . (s_span &&& id)) . M.keys . (^. (ps_binds . bg_bnd_app_map))) pts
          bound_fns :: STree.STree [STInterval.Interval (Locd LIdentifier)] (Locd LIdentifier)
          bound_fns = STree.fromList bound_fns'
          
          i_targ = read s_targ :: Int
          targ_fold css@(cs, _) l = 
            let next = if Prof.costCentreNo cs == i_targ then Just css else Nothing
                l' = mconcat $ map (fmap First . bisequence . second pure) l
            in case l' of -- pass up entire subtree if we haven't found the module yet, otherwise constrain to single branch
              Just (First (cs_matched, t)) -> (Just cs_matched, Tr.Node css [t])
              Nothing -> (next, Tr.Node css (map snd l))
          (targs, targ_ts) = unzip $ mapMaybe (bisequence . second pure . Tr.foldTree targ_fold) loc_cs_forest
          -- targ_segs' = SegTree $ STree.fromList $ concatMap (Tr.foldTree ((.concat) . (:) . seg2intervalish . second (const ()) . swap)) targ_ts
          targ_segs = SegTree $ STree.fromList $ concatMap (Tr.foldTree ((.concat) . (:) . seg2intervalish . uncurry Spand . swap)) targ_ts
          bks = 
            (map (Left . BindNamed) $ ivl_payloads $ concatMap (STree.superintervalQuery bound_fns . span2interval . snd) targs)
            <> ([
                Right ident
                | (_targn, targ_span) <- targs
                , pt <- pts
                , idents <- segfind (pt ^. ps_app_groups ^. ag_span_map) targ_span
                , ident <- s_payload idents
              ])
          grs = [
              pt_search dflags pt targ_segs bk
              | bk <- bks
              , pt <- pts
            ]
          hollow_state = uncurry HollowGrState $ mconcat
            $ zipWith (\k gr -> (map (k,) *** (uncurry JSGraph . (gr2adjlist (k,) &&& map (map (k,)) . Gr.scc) . Gr.emap (hollow_span . s_span) . Gr.nmap (nk_ctor &&& hollow_span . nk_span))) gr) [0..] grs
      in flip const (targs, bound_fns, dflags, loc_cs_forest, pts, grs) $ do -- $ trace (show $ length loc_cs_forest) -- const (putStrLn $ unlines $ head $ M.elems $ lined_srcs) 
        writeFile "static/gr.json" (UBL.toString $ Aeson.encode hollow_state)
        const (pure ()) $ do
          -- putStrLn $ ppr_safe dflags $ M.elems . (^. ps_fns) <$> pts
          -- putStrLn $ ppr_safe dflags $ (^. (ps_binds . bg_named_lookup)) <$> pts
          -- ppr_ dflags bound'
          -- putStrLn $ show $ [(b, sp, b == sp) | (_, sp) <- targs, b <- (bound''' M.! ("Main", "merge_decomps"))]
          const (pure ()) $ print $ concatMap (\(_, s) -> ivl_locs $ STree.superintervalQuery bound_fns $ span2interval s) targs
          -- print bound''
          -- putStrLn $ ppr_safe dflags bound'
          print (length bks, map ((length . (_ag_ident_map . _ps_app_groups) &&& length . (^. (ps_binds . bg_bnd_app_map)))) pts)
          -- putStrLn $ Tr.drawForest $ fmap (show . snd) <$> targ_ts
          -- ppr_ dflags $ map (S.fromList . concat . M.elems . _ag_ident_map . _ps_app_groups) pts
          -- -- putStrLn $ ppr_safe dflags bound -- <$> (bound M.!? targs)
          -- print $ sum $ map (length . filter id . xselfmap (curry $ not . uncurry (||) . (uncurry (==) &&& (S.null . uncurry (S.\\) . both (S.fromList . s_payload)))) . concat . M.elems . (^. (ps_app_groups . ag_ident_map))) pts
          const (pure ()) $ putStrLn $ unlines $ map (\(n, gr) ->
              unlines $ map (uncurry ((++) . (++"->")) . both (fromMaybe "" . fmap (ppr_nk dflags) . Gr.lab gr)) $ unique $ Gr.edges gr
            ) grs
          putStrLn $ unlines $ map (ppr_safe dflags . fst) $ grs
          putStrLn $ unlines $ map (\(ns, gr) -> unlines $ map (
              unlines . Tr.foldTree (
                  curry $ uncurry (:) . (
                      fromMaybe "<N>"
                      . (
                          fmap (uncurry (<>) . (
                              (<>" ") . nk_ctor . fst
                              &&& concat . intersperse "; " . map (dropWhile (==' ')) . uncurry snip_src . first nk_span)
                            )
                          . ((bisequence . (
                              Just
                              &&& (lined_srcs M.!?) . unpackFS . srcSpanFile . nk_span
                            )) . Gr.lab')
                          =<<
                        ) . fst . flip Gr.match gr
                      *** concatMap (map (' ':))
                    )
                )
            ) $ Gr.dff ns gr) grs
          {- $ map (\case -- gr_prettify (ppr_safe dflags) id
              (Just node, gr) -> gr_prettify (ppr_safe dflags) id gr
              (Nothing, _) -> mempty
            ) grs -}
          