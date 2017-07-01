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


type Engine =
  FilePath -> FilePath -> [String] -> IO (Either String FilePath)

lualatex :: Engine
lualatex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/latexmk" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> "htex-job.pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "-lualatex"
        ,"-f"
        ,"-output-directory=" <> outDir
        ,"-jobname=htex-job" ]
        ++ extraArgs ++ [ texfile ]

pdflatex :: Engine
pdflatex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/pdflatex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> "htex-job.pdf"
    ExitFailure _ -> Left out
  where
    args = 
        [ "--interaction=scrollmode"
        ,"--output-directory=" <> outDir
        ,"--jobname=htex-job"
        ,"--file-line-error" ]
        ++ extraArgs ++ [ texfile ]


compile :: Engine
  -> FilePath
  -> FilePath
  -> MVar String
  -> [String]
  -> IO ThreadId
compile engine texfile pdffile mvar extraArgs =
  forkIO $ withSystemTempDirectory "htex"
    $ \dir -> do
      copyFile texfile (dir </> texfile)
      engine dir texfile extraArgs >>= \case
        Left err -> putMVar mvar err
        Right outFile -> do
          copyFile outFile pdffile
          putMVar mvar $ "Successful build from " <> dir



