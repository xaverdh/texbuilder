module TexBuilder.CompileThread
  ( compileThread
  , compileThreadDir )
where

import TexBuilder.Utils
import TexBuilder.Engine

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad
import Control.Monad.Extra
import System.INotify
import Control.Concurrent.MVar


compileThreadDir :: FilePath -- ^ Path of the directory to watch
  -> IO PP.Doc -- ^ The code compilation action
  -> BinSem
  -- ^ Signaling semaphore to communicate when the
  --   pdf view should be updated.
  -> IO ()
compileThreadDir dir run sem = do
  mvar <- newEmptyMVar
  withINotify $ \inotify ->
     let watch = void . addWatch inotify [Modify,Create] dir
      in compileThreadDir' run watch sem mvar


compileThreadDir' :: IO PP.Doc
  -> ((Event -> IO ()) -> IO ())
  -> BinSem
  -> MVar PP.Doc
  -> IO ()
compileThreadDir' run watch sem mvar =
  watch go >> logLoop
  where
    go event = do
      case event of
        Modified False mbPath -> whenJust mbPath
          $ \path -> when (isTexFile path) go'
        Created False path -> when (isTexFile path) go'
        Ignored -> go' >> watch go
        _ -> pure ()

    go' = void $ do
      res <- run
      putMVar mvar res
      signal sem

    logLoop  = do
      takeMVar mvar >>= PP.putDoc
      logLoop



compileThread :: FilePath -- ^ Path of the tex file
  -> IO PP.Doc -- ^ The code compilation action
  -> BinSem
  -- ^ Signaling semaphore to communicate when the
  --   pdf view should be updated.
  -> IO ()
compileThread texfile run sem = do
  mvar <- newEmptyMVar
  withINotify $ \inotify ->
     let watch = void . addWatch inotify [Modify] texfile
      in compileThread' run watch sem mvar


compileThread' :: IO PP.Doc
  -> ((Event -> IO ()) -> IO ())
  -> BinSem
  -> MVar PP.Doc
  -> IO ()
compileThread' run watch sem mvar =
  watch go >> logLoop
  where
    go event = do
      res <- run
      putMVar mvar res
      signal sem
      case event of
        Ignored -> watch go
        _ -> pure ()

    logLoop  = do
      takeMVar mvar >>= PP.putDoc
      logLoop




