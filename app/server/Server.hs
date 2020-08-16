{-# LANGUAGE OverloadedStrings #-}
import Happstack.Server ( Response(..), ServerPart(..), Method(..), ToMessage(..), decodeBody, defaultBodyPolicy, dir, look, nullConf, simpleHTTP, method, toResponse, toResponseBS, ok, badRequest, serveDirectory, Browsing(..) )
import qualified Happstack.Server as HS
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )
import Control.Exception ( SomeException(..), try, tryJust, catch )
import Data.ByteString.Lazy.UTF8 as LUB ( fromString )
import Data.ByteString.Lazy as LB ( fromStrict )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( msum )
import System.FilePath ( (</>) )
import NameCache ( NameCache(..) )
import System.Environment ( getArgs )
import Data.Either ( rights )

import HieBin
import HieTypes
import HieUtils

import Stacker.Util

parseHIE_ep :: [FilePath] -> IORef NameCache -> ServerPart Response
parseHIE_ep hiedirs m_nc = do
  method GET
  fn <- look "n"
  s <- liftIO $ do
    nc <- readIORef m_nc
    mapM (\hiedir -> try $ do
        (f_hie, nc') <- readHieFile nc (hiedir </> fn)
        writeIORef m_nc nc'
        return f_hie
      ) hiedirs
  case rights (s :: [Either SomeException HieFileResult]) of
    f_hie:_ -> ok $ toResponseBS "text/plain" $ LB.fromStrict $ hie_hs_src $ hie_file_result f_hie
    [] -> badRequest $ toResponseBS "text/plain" (LUB.fromString fn <> " not found.")

main :: IO ()
main = do
  dflags <- dynFlagsForPrinting
  nc <- makeNc
  m_nc <- newIORef nc
  hiedirs <- getArgs
  
  putStrLn "Listening..."
  simpleHTTP nullConf $ msum [
      dir "f" (parseHIE_ep hiedirs m_nc)
      , dir "static" $ serveDirectory EnableBrowsing [] "./static"
      , serveDirectory DisableBrowsing ["index.html"] "./web/public"
    ]