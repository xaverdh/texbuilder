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
    "/usr/bin/lualatex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "--interaction=scrollmode"
        ,"--output-directory=" <> outDir
        ,"--jobname=" <> jobname
        ,"--file-line-error" ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"

pdflatex :: Engine
pdflatex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/pdflatex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args = 
        [ "--interaction=scrollmode"
        ,"--output-directory=" <> outDir
        ,"--jobname=" <> jobname
        ,"--file-line-error" ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"


lualatexLatexMk :: Engine
lualatexLatexMk outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/latexmk" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "-lualatex"
        ,"-f"
        ,"-output-directory=" <> outDir
        ,"-jobname=" <> jobname ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"


compile :: Engine
  -> FilePath
  -> FilePath
  -> MVar String
  -> [String]
  -> IO ThreadId
compile engine texfile pdffile mvar extraArgs =
  forkIO $ withSystemTempDirectory "texbuilder"
    $ \dir -> do
      copyFile texfile (dir </> texfile)
      engine dir texfile extraArgs >>= \case
        Left err -> putMVar mvar err
        Right outFile -> do
          copyFile outFile pdffile
          putMVar mvar $ "Successful build from " <> dir



