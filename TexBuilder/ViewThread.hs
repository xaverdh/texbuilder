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
  tid <- forkIO $ withINotify $ \inotify -> do
    newEmptyMVar >>= watch inotify pid
  exCode <- waitForProcess ph
  killThread tid
  where
    watch inotify pid mvar = do
      wdesc <- addWatch inotify
        [Attrib]
        pdffile
        (const $ void $ tryPutMVar mvar ())
      takeMVar mvar
      signalProcess sigHUP pid
      newEmptyMVar >>= watch inotify pid


getPid :: ProcessHandle -> IO (Maybe CPid)
getPid = flip withProcessHandle $ \ph_ ->
  case ph_ of
    OpenHandle x   -> return $ Just x
    ClosedHandle _ -> return Nothing




