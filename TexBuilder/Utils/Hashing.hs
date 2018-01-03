{-# LANGUAGE PackageImports #-}
module TexBuilder.Utils.Hashing
  ( computeHash
  , computeHashes
  , module Crypto.Hash )
where

import Control.Monad
import Control.Monad.Trans
import Control.DeepSeq

import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Lazy as LB
-- import qualified Data.ByteString as B

computeHash :: (MonadIO io,HashAlgorithm a)
  => FilePath -> io (Digest a)
computeHash path = do
  hash <- liftIO (hashlazy <$> LB.readFile path)
  pure $ deepseq hash hash
  -- ^ deepseq is neccessary to force reading to
  --   actually happen here (we want to capture the
  --   _current_ state of the file).

computeHashes ::
  (MonadIO io,HashAlgorithm a,Traversable t,NFData (t (Digest a)))
  => t FilePath -> io (t (Digest a))
computeHashes paths = do
  hashes <- forM paths $ \path ->
    liftIO (hashlazy <$> LB.readFile path)
  pure $ deepseq hashes hashes
  -- ^ deepseq is neccessary to force reading to
  --   actually happen here (we want to capture the
  --   _current_ state of the file).


