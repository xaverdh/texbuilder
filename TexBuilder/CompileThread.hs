module TexBuilder.CompileThread
  ( compileThread )
where

import TexBuilder.Engine

import Control.Monad
import System.INotify
import Control.Concurrent.MVar



compileThread :: FilePath -> FilePath -> Engine -> IO ()
compileThread texfile pdffile engine =
  withINotify $ \inotify -> do
    mvar <- newEmptyMVar
    watch inotify $ go inotify mvar
    logLoop mvar
  where
    watch i = addWatch i [Modify] texfile

    go inotify mvar event = do
      compile engine texfile pdffile mvar
      case event of
        Ignored -> void $ watch inotify $ go inotify mvar
        _ -> pure ()

    logLoop mvar = do
      out <- takeMVar mvar
      putStrLn out
      logLoop mvar




