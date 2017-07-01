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
import qualified Data.ByteString.Lazy as LB
import Crypto.Hash

import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.MVar

import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Temp
import System.Environment (setEnv)
import System.Posix.Time (epochTime)

type Engine =
  FilePath -> FilePath -> [String] -> IO (Either String FilePath)

data RecompileState =
  StInitial Natural
  | StSucc Natural FilePath (Digest MD5)

recompile :: Natural -> Engine -> Engine
recompile maxNum engine outDir texfile extraArgs = do
  time <- show <$> epochTime
  evalStateT (recompileSt (run time)) (StInitial maxNum)
  where
    run time = do
      setEnv "SOURCE_DATE_EPOCH" time
      -- ^ For reproductible output
      engine outDir texfile extraArgs


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
        hash <- lift (hashlazy <$> LB.readFile path)
        k path hash
    succ i path hash = do
      put $ StSucc (i-1) path hash
      recompileSt run
    done = pure . Right
    failed = pure . Left


luaLaTex :: Engine
luaLaTex outDir texfile extraArgs = do
  putStrLn "Note that lualatex does not currently respect \
          \SOURCE_DATE_EPOCH, so the source will be \
          \rebuild the maximum number of times, slowing \
          \things down."
  (exCode,out,err) <- readProcessWithExitCode
    "/usr/bin/lualatex" args ""
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
    "/usr/bin/pdflatex" args ""
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
        [ "-lualatex"
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






