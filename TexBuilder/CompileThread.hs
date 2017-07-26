{-# language PackageImports, LambdaCase #-}
module TexBuilder.CompileThread
  ( compileThread
  , compileThreadDir )
where

import TexBuilder.Utils
import TexBuilder.Engine

import qualified Text.PrettyPrint.ANSI.Leijen as PP

import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Extra
import Control.Monad.State
import Control.DeepSeq

import System.INotify
import Control.Concurrent.MVar



data CompileLoopState = CompileLoopState
  { hashes :: M.Map FilePath (Digest MD5)
  , watchMVar :: MVar FilePath }

mkCLS = CompileLoopState M.empty


compileThreadDir :: FilePath -- ^ Path of the directory to watch
  -> IO PP.Doc -- ^ The code compilation action
  -> BinSem
  -- ^ Signaling semaphore to communicate when the
  --   pdf view should be updated.
  -> IO ()
compileThreadDir dir run sem = do
  withINotify $ \inotify ->
     let watch = void . addWatch inotify [Modify,Create] dir
      in compileThreadDir' run watch sem


compileThreadDir' :: IO PP.Doc
  -> ((Event -> IO ()) -> IO ())
  -> BinSem
  -> IO ()
compileThreadDir' run watch viewSem = do
  wMVar <- newEmptyMVar
  watch (watcherThread wMVar watch)
  evalStateT compileLoop $ mkCLS wMVar
  where
    compileLoop = do
      path <- lift . takeMVar =<< gets watchMVar
      -- ^ Wait for watcher thread to signal potential changes
      newHash <- lift (hashlazy <$> LB.readFile path)
      -- ^ Hash the file in question
      deepseq newHash $ do
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

watcherThread :: MVar FilePath
  -> ((Event -> IO ()) -> IO ())
  -> Event -> IO ()
watcherThread wMVar watch = \case
  Modified False mbPath ->
    whenJust mbPath $ \path ->
      when (isTexFile path) $ putMVar wMVar path
  Created False path ->
    when (isTexFile path) $ putMVar wMVar path
  Ignored -> watch (watcherThread wMVar watch)
  _ -> pure ()



compileThread :: FilePath -- ^ Path of the tex file
  -> IO PP.Doc -- ^ The code compilation action
  -> BinSem
  -- ^ Signaling semaphore to communicate when the
  --   pdf view should be updated.
  -> IO ()
compileThread texfile run sem = do
  mvar <- newEmptyMVar
  withINotify $ \inotify ->
     let watch = void . addWatch inotify [Modify] texfile
      in compileThread' run watch sem mvar


compileThread' :: IO PP.Doc
  -> ((Event -> IO ()) -> IO ())
  -> BinSem
  -> MVar PP.Doc
  -> IO ()
compileThread' run watch sem mvar =
  watch go >> logLoop
  where
    go event = do
      res <- run
      putMVar mvar res
      signal sem
      case event of
        Ignored -> watch go
        _ -> pure ()

    logLoop  = do
      takeMVar mvar >>= PP.putDoc
      logLoop




