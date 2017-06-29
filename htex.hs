module Main where

import Control.Monad
import Data.Monoid
import Data.Maybe

import Options.Applicative
import Options.Applicative.Builder
import System.Directory
import System.FilePath
import System.INotify
import System.Process
import System.Process.Internals
import System.Exit
import System.Posix.Types
import System.Posix.Signals
import System.IO.Temp
import Control.Concurrent
import Control.Concurrent.MVar

main :: IO ()
main = do
  action <- execParser parser
  action
  where
    parser = info opts idm
    opts = htex
      <$> option str
        ( short 't' <> long "tex" <> metavar "TEXFILE" )
      <*> option (Just <$> str)
        ( short 'p' <> long "pdf" <> metavar "PDFFILE" <> value Nothing )
      <*> flag lualatex pdflatex ( long "noluatex" )


htex :: FilePath -> Maybe FilePath -> Engine -> IO ()
htex texfile mbf engine = 
  let pdffile = fromMaybe (texfile -<.> "pdf") mbf
   in do
    setupTexFile texfile
    forkOS $ onFileEx pdffile (mupdfView pdffile)
    compileTex texfile pdffile engine

type Engine = FilePath -> FilePath -> IO (ExitCode,String,String)


lualatex :: Engine
lualatex dir texfile = readProcessWithExitCode "latexmk"
  [ "-lualatex"
   ,"-f"
   ,"-output-directory=" <> dir
   ,"-jobname=htex-job"
   ,texfile ] ""

pdflatex :: Engine
pdflatex dir texfile = readProcessWithExitCode "pdflatex"
  [ "--interaction=scrollmode"
   ,"--output-directory=" <> dir
   ,"--jobname=htex-job"
   ,"--file-line-error"
   ,texfile ] ""

compileTex :: FilePath -> FilePath -> Engine -> IO ()
compileTex texfile pdffile engine = do
  inotify <- initINotify
  mvar <- newEmptyMVar
  wdesc <- addWatch inotify
    [CloseWrite]
    texfile
    (const $ void $ go mvar)
  logLoop mvar
  where
    go mvar = forkIO
      $ withSystemTempDirectory "htex"
        $ \dir -> do
          copyFile texfile (dir </> texfile)
          (exCode,out,err) <- engine dir texfile
          case exCode of
            ExitSuccess -> do
              copyFile (dir </> "htex-job.pdf") pdffile
              putMVar mvar $
                "Successful build from " <> dir
            ExitFailure _ -> putMVar mvar out
  
    logLoop mvar = do
      out <- takeMVar mvar
      putStrLn out
      logLoop mvar


setupTexFile :: FilePath -> IO ()
setupTexFile texfile = do
  ex <- doesFileExist texfile
  unless ex $ do
    putStrLn (texfile <> " does not exist.")
    exitFailure


onFileEx :: FilePath -> IO a -> IO a
onFileEx file action = do
  ex <- doesFileExist file
  case ex of
    True -> action
    False -> do
      inotify <- initINotify
      mvar <- newMVar False
      wdesc <- addWatch inotify
        [Create]
        (takeDirectory file)
        (onCreate mvar)
      takeMVar mvar
      removeWatch wdesc
      action
  where
    onCreate mvar _ = do
      ex <- doesFileExist file
      when ex $ do
        putMVar mvar True

mupdfView :: FilePath -> IO ()
mupdfView pdffile = do
  ph <- spawnProcess "mupdf" [pdffile]
  Just pid <- getPid ph
  inotify <- initINotify
  newEmptyMVar >>= watch inotify pid
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

