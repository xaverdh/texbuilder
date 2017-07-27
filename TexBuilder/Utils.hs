{-# language PackageImports #-}
module TexBuilder.Utils where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans
import Control.DeepSeq

import System.INotify
import System.Directory
import System.FilePath

import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Monoid
import Numeric.Natural

withHash :: (MonadIO io,HashAlgorithm a)
  => FilePath -> (Digest a -> io b) -> io b
withHash path k = do
  hash <- liftIO (hashlazy <$> LB.readFile path)
  deepseq hash $ k hash
  -- ^ deepseq is neccessary to force reading to
  --   actually happen here (we want to capture the
  --   _current_ state of the file).

withHashes ::
  (MonadIO io,HashAlgorithm a,Traversable t,NFData (t (Digest a)))
  => t FilePath -> (t (Digest a) -> io b) -> io b
withHashes paths k = do
  hashes <- forM paths $ \path ->
    liftIO (hashlazy <$> LB.readFile path)
  deepseq hashes $ k hashes
  -- ^ deepseq is neccessary to force reading to
  --   actually happen here (we want to capture the
  --   _current_ state of the file).


listDirAbsolute :: FilePath -> IO [FilePath]
listDirAbsolute dir = do
  dir' <- makeAbsolute dir
  map (dir'</>) <$> listDirectory dir'

listSubdirs :: Natural -> FilePath -> IO [FilePath]
listSubdirs 0 _ = pure []
listSubdirs depth dir = do
  paths <- listDirAbsolute dir
  dirs <- filterM doesDirectoryExist paths
  subdirs <- forM dirs $ listSubdirs (depth-1)
  pure $ dirs <> join subdirs

searchFilesWith :: (FilePath -> Bool) -> [FilePath] -> IO [FilePath]
searchFilesWith f dirs = do
  paths <- join <$> forM dirs listDirAbsolute
  pure $ filter f paths


type BinSem = MVar ()

newBinSem :: IO BinSem
newBinSem = newEmptyMVar

wait :: BinSem -> IO ()
wait = takeMVar 

signal :: BinSem -> IO ()
signal = flip putMVar ()

trySignal :: BinSem -> IO Bool
trySignal = flip tryPutMVar ()

trySignal' :: BinSem -> IO ()
trySignal' = void . trySignal


-- Be careful with this stuff.
-- Inotify is _inode_ based, but these functions want
-- to watch _paths_. So they reregister the watches
-- when necessary. Still it is probably best to look
-- at the implementation before using them.
-- Also this whole business is rather error prone..

-- ^ Simple version of /withEvent/
onEvent :: [EventVariety] -> FilePath -> IO a -> IO ()
onEvent variety file k = withEvent variety file (const k)

-- ^ Execute action once an event involving the given
--   filepath happens.
--   This function blocks.
withEvent :: [EventVariety]
  -> FilePath
  -> (Event -> IO a)
  -> IO ()
withEvent variety file k =
  withINotify $ \inotify -> do
    sem <- newBinSem
    wd <- addWatch inotify variety file
      (k >=> const (trySignal' sem))
    wait sem
    removeWatch wd
  
-- ^ Simple version of /loopWithEvent/
loopOnEvent :: [EventVariety] -> FilePath -> IO Bool -> IO ()
loopOnEvent variety file k = loopWithEvent variety file (const k)

-- ^ Execute action every time an Event involving the given
--   filepath happens untils the callback returns True.
--   This function blocks.
loopWithEvent :: [EventVariety] -- ^ Events to watch
  -> FilePath -- ^ Filepath to watch
  -> (Event -> IO Bool)
  -- ^ Callback. If it returns True we stop looping
  -> IO ()
loopWithEvent variety file k = 
  withINotify $ \inotify -> do
    doneMVar <- newEmptyMVar
    loopWithEventImpl variety file k inotify doneMVar

loopWithEventImpl :: [EventVariety]
  -> FilePath
  -> (Event -> IO Bool)
  -> INotify
  -> MVar Bool
  -> IO ()
loopWithEventImpl variety file k inotify doneMVar = loop
  where
    loop = do
      wd <- watch
      done <- takeMVar doneMVar
      removeWatch wd
      unless done loop
    watch = addWatch inotify variety file
            (k >=> putMVar doneMVar)

waitForFile :: FilePath -> IO ()
waitForFile file = 
  loopOnEvent [Create] dir (doesFileExist file)
  where dir = takeDirectory file

onFileEx :: FilePath -> IO a -> IO a
onFileEx file action =
  unlessM
    (doesFileExist file)
    (waitForFile file)
  >> action



