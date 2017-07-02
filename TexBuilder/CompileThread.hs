module TexBuilder.CompileThread
  ( compileThread )
where

import TexBuilder.Engine

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Control.Monad
import System.INotify
import Control.Concurrent.MVar


compileThread :: FilePath
  -> FilePath
  -> Engine
  -> [String]
  -> IO ()
compileThread texfile pdffile engine extraArgs =
  withINotify $ \inotify -> do
    mvar <- newEmptyMVar
    watch inotify $ go inotify mvar
    logLoop mvar
  where
    watch i = addWatch i [Modify] texfile

    go inotify mvar event = do
      compile engine texfile pdffile mvar extraArgs
      case event of
        Ignored -> void $ watch inotify $ go inotify mvar
        _ -> pure ()

    logLoop mvar = do
      takeMVar mvar >>= PP.putDoc
      logLoop mvar




