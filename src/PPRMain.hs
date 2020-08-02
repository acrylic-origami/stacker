{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, RecordWildCards, TemplateHaskell, LambdaCase, TupleSections, Rank2Types, MultiWayIf #-}

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Prof as Prof
import SrcLoc ( SrcLoc(..), SrcSpan, RealSrcSpan(..), mkSrcLoc, mkSrcSpan, containsSpan, mkRealSrcSpan, mkRealSrcLoc )
import Data.Bitraversable ( bisequence )
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
import Data.Maybe ( catMaybes, fromMaybe, mapMaybe, maybeToList )
import Data.List ( uncons )
import Data.Foldable ( foldrM, foldlM )
import Control.Arrow ( (***), (&&&), first, second )
import Data.Semigroup ( Last(..) )
import Control.Applicative ( liftA2 )
import UniqSupply ( mkSplitUniqSupply )
import Control.Lens ( makeLenses )
import Control.Monad.State ( MonadState(..), State(..), StateT(..) )
import qualified Control.Lens as L
import Control.Lens.Operators

import qualified Data.SegmentTree as STree

import System.Directory
import System.FilePath

import Outputable ( showSDoc, interppSP, Outputable(..) )

import SysTools ( initSysTools )
import DynFlags ( DynFlags, defaultDynFlags )
import GHC.Paths (libdir)

dynFlagsForPrinting :: IO DynFlags
dynFlagsForPrinting = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings ([], [])

ppr_unsafe :: Outputable a => DynFlags -> a -> String
ppr_unsafe d = showSDoc d . interppSP . pure

makeNc :: IO NameCache
makeNc = do
  uniq_supply <- mkSplitUniqSupply 'z'
  return $ initNameCache uniq_supply []

-- generously donated by HieDb
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

ppr_ast :: DynFlags -> HieAST a -> String
ppr_ast d = uncurry ((++) . (++"\n")) . (ppr_unsafe d . ((nodeAnnotations &&& M.map (identInfo) . nodeIdentifiers) . nodeInfo &&& nodeSpan) &&& unlines . map (ppr_ast d) . nodeChildren)

main :: IO ()
main = do
  dflags <- dynFlagsForPrinting
  nc <- makeNc
  (fprof:dhies) <- getArgs
  fhies <- fmap concat $ mapM getHieFilesIn $ dhies
  (hies, _) <- foldrM (\f (l, nc') -> (first ((:l) . hie_file_result)) <$> readHieFile nc' f) ([], nc) fhies
  
  putStrLn $ unlines $ map (ppr_ast dflags) $ M.elems $ getAsts $ hie_asts $ head hies