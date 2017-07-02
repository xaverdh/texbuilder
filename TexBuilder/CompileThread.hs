module TexBuilder.CompileThread
  ( compileThread )
where

import TexBuilder.Utils
import TexBuilder.Engine

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad
import System.INotify
import Control.Concurrent.MVar


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




