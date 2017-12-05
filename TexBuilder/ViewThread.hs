module TexBuilder.ViewThread
  ( mupdfView )
where

import TexBuilder.Utils.BinSem

import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Concurrent
import System.Posix.Types
import System.Posix.Signals
import System.Process
import System.Process.Internals
import System.Directory


mupdfView :: FilePath -- ^ The path of the file to view
  -> BinSem
  -- ^ Signaling semaphore to communicate when the
  --   pdf view should be updated.
  -> IO ()
mupdfView pdffile sem = do
  ph <- do
    mupdfPath <- getMupdfPath
    spawnProcess mupdfPath [pdffile]
  Just pid <- getPid ph
  tid <- forkIO $ signalThread pid sem
  exCode <- waitForProcess ph
  killThread tid
  where
    getMupdfPath :: IO FilePath
    getMupdfPath = fromMaybe (error "mupdf not found")
      . foldr (<|>) Nothing
      <$> mapM findExecutable [ "mupdf", "mupdf-x11" ]

signalThread :: ProcessID -> BinSem -> IO ()
signalThread pid sem = do
  wait sem
  signalProcess sigHUP pid
  signalThread pid sem 
  
getPid :: ProcessHandle -> IO (Maybe CPid)
getPid = flip withProcessHandle $ \ph_ ->
  case ph_ of
    OpenHandle x   -> pure $ Just x
    ClosedHandle _ -> pure Nothing




