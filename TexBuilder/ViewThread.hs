module TexBuilder.ViewThread
  ( mupdfView )
where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.Posix.Types
import System.Posix.Signals
import System.Process
import System.Process.Internals
import System.INotify



mupdfView :: FilePath -> IO ()
mupdfView pdffile = do
  ph <- spawnProcess "/usr/bin/mupdf" [pdffile]
  Just pid <- getPid ph
  tid <- forkIO $
    onFileTouched pdffile (signalProcess sigHUP pid)
  exCode <- waitForProcess ph
  killThread tid


onFileTouched :: FilePath -> IO a -> IO ()
onFileTouched file action =
  withINotify $ \inotify -> do
    mvar <- newEmptyMVar
    wdesc <- addWatch inotify
      [Attrib] file
      (const $ void $ putMVar mvar ())
    loop mvar
  where
    loop mvar = do
      takeMVar mvar
      action
      loop mvar

getPid :: ProcessHandle -> IO (Maybe CPid)
getPid = flip withProcessHandle $ \ph_ ->
  case ph_ of
    OpenHandle x   -> return $ Just x
    ClosedHandle _ -> return Nothing




