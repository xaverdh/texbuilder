{-# language LambdaCase #-}
module TexBuilder.Engine
  ( Engine(..)
  , compile
  , luaLaTex
  , pdfLaTex
  , recompile
  , luaLaTexMk
  , pdfLaTexMk )
where

import Data.Monoid
import Numeric.Natural
import Data.ByteString.Lazy as LB
import Data.Digest.XXHash

import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.MVar

import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Temp


type Engine =
  FilePath -> FilePath -> [String] -> IO (Either String FilePath)

data RecompileState =
  StInitial Natural
  | StSucc Natural FilePath XXHash

recompile :: Natural -> Engine -> Engine
recompile maxNum engine outDir texfile extraArgs =
  let run = engine outDir texfile extraArgs
   in evalStateT (recompileSt run) (StInitial maxNum)

recompileSt :: IO (Either String FilePath)
  -> StateT RecompileState IO (Either String FilePath)
recompileSt run = get >>= \case
  StInitial maxNum -> go $ succ maxNum
  StSucc i path oldHash ->
    if i <= 0
      then done path
      else go $ \path hash ->
        if hash == oldHash
          then done path
          else succ i path hash
  where
    go k = lift run >>= \case
      Left err -> failed err
      Right path -> do
        hash <- lift (xxHash <$> LB.readFile path)
        k path hash
    succ i path hash = do
      put $ StSucc (i-1) path hash
      recompileSt run
    done = pure . Right
    failed = pure . Left



luaLaTex :: Engine
luaLaTex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/luaLaTex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "--interaction=scrollmode"
        , "--output-directory=" <> outDir
        , "--jobname=" <> jobname
        , "--output-format=pdf"
        , "--file-line-error" ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"

pdfLaTex :: Engine
pdfLaTex outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/pdfLaTex" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args = 
        [ "--interaction=scrollmode"
        , "--output-directory=" <> outDir
        , "--jobname=" <> jobname
        , "--output-format=pdf"
        , "--file-line-error" ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"


luaLaTexMk :: Engine
luaLaTexMk outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/latexmk" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "-luaLaTex"
        , "-f"
        , "-output-directory=" <> outDir
        , "-jobname=" <> jobname ]
        ++ extraArgs ++ [ texfile ]
    jobname = "texbuilder-job"

pdfLaTexMk :: Engine
pdfLaTexMk outDir texfile extraArgs = do
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/latexmk" args ""
  pure $ case exCode of
    ExitSuccess -> Right $ outDir </> jobname <.> "pdf"
    ExitFailure _ -> Left out
  where
    args =
        [ "-pdf"
        , "-f"
        , "-output-directory=" <> outDir
        , "-jobname=" <> jobname ]
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



