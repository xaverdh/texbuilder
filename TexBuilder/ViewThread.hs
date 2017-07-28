module TexBuilder.ViewThread
  ( mupdfView )
where

import TexBuilder.Utils.BinSem

import Data.Functor
import Control.Monad
import Control.Concurrent
import System.Posix.Types
import System.Posix.Signals
import System.Process
import System.Process.Internals



mupdfView :: FilePath -- ^ The path of the file to view
  -> BinSem
  -- ^ Signaling semaphore to communicate when the
  --   pdf view should be updated.
  -> IO ()
mupdfView pdffile sem = do
  ph <- spawnProcess "/usr/bin/mupdf" [pdffile]
  Just pid <- getPid ph
  tid <- forkIO $ signalThread pid sem
  exCode <- waitForProcess ph
  killThread tid

signalThread :: ProcessID -> BinSem -> IO ()
signalThread pid sem = do
  wait sem
  signalProcess sigHUP pid
  signalThread pid sem 
  
getPid :: ProcessHandle -> IO (Maybe CPid)
getPid = flip withProcessHandle $ \ph_ ->
  case ph_ of
    OpenHandle x   -> return $ Just x
    ClosedHandle _ -> return Nothing




