module TexBuilder.Utils.BinSem where

import Control.Concurrent.MVar
import Control.Monad


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




