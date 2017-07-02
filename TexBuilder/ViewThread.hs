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
onFileTouched file action = do
  mvar <- newEmptyMVar
  withINotify $ onFileTouched' file action mvar

onFileTouched' :: FilePath -> IO a -> MVar () -> INotify -> IO ()
onFileTouched' file action mvar inotify = loop
  where
    watch = void . addWatch inotify [Attrib] file
    go _ = void $ tryPutMVar mvar ()
    
    loop = do
      watch go
      takeMVar mvar
      action
      loop
    
getPid :: ProcessHandle -> IO (Maybe CPid)
getPid = flip withProcessHandle $ \ph_ ->
  case ph_ of
    OpenHandle x   -> return $ Just x
    ClosedHandle _ -> return Nothing




