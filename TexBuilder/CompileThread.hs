{-# language PackageImports, LambdaCase #-}
module TexBuilder.CompileThread
  ( compileThread )
where

import TexBuilder.Utils
import TexBuilder.Engine
import TexBuilder.Watches

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Extra
import Control.Monad.State

import System.INotify
import Control.Concurrent.MVar



data CompileLoopState = CompileLoopState
  { hashes :: M.Map FilePath (Digest MD5)
  , watchMVar :: MVar FilePath }

mkCLS = CompileLoopState M.empty


compileThread :: IO PP.Doc -- ^ The code compilation action
  -> BinSem
  -- ^ Signaling semaphore to communicate when the
  --   pdf view should be updated.
  -> MVar FilePath
  -- ^ Communication MVar for signaling file changes
  -> IO ()
compileThread run viewSem wMVar =
  evalStateT compileLoop $ mkCLS wMVar
  where
    compileLoop = do
      path <- lift . takeMVar =<< gets watchMVar
      -- ^ Wait for watcher thread to signal potential changes
      withHash path $ \newHash -> do
        table <- gets hashes
        case M.lookup path table of -- ^ lookup old hash
          Nothing -> go
          Just oldHash -> when (oldHash /= newHash) go
        -- ^ Compile when file was changed
        modify $ \st -> st { hashes = M.insert path newHash table }
        -- ^ Write new hash value
        compileLoop
    
    go = lift $ do
      run >>= PP.putDoc
      signal viewSem


