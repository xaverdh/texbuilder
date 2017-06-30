{-# language LambdaCase #-}
module TexBuilder.Engine
  ( Engine(..)
  , compile
  , lualatex
  , pdflatex )
where

import Data.Monoid

import Control.Concurrent
import Control.Concurrent.MVar

import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Temp


type Engine = FilePath -> FilePath -> IO (Either String FilePath)

lualatex :: Engine
lualatex outDir texfile = do
  (exCode,out,err) <- 
    readProcessWithExitCode "/usr/bin/latexmk"
      [ "-lualatex"
       ,"-f"
       ,"-output-directory=" <> outDir
       ,"-jobname=htex-job"
       ,texfile ] ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> "htex-job.pdf"
    ExitFailure _ -> Left out

pdflatex :: Engine
pdflatex outDir texfile = do
  (exCode,out,err) <-
    readProcessWithExitCode "/usr/bin/pdflatex"
      [ "--interaction=scrollmode"
       ,"--output-directory=" <> outDir
       ,"--jobname=htex-job"
       ,"--file-line-error"
       ,texfile ] ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> "htex-job.pdf"
    ExitFailure _ -> Left out

compile :: Engine
  -> FilePath
  -> FilePath
  -> MVar String
  -> IO ThreadId
compile engine texfile pdffile mvar = forkIO
  $ withSystemTempDirectory "htex"
    $ \dir -> do
      copyFile texfile (dir </> texfile)
      engine dir texfile >>= \case
        Left err -> putMVar mvar err
        Right outFile -> do
          copyFile outFile pdffile
          putMVar mvar $ "Successful build from " <> dir



