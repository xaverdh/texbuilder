{-# LANGUAGE PackageImports #-}
module TexBuilder.Utils.Hashing
  ( withHash
  , withHashes
  , module Crypto.Hash )
where

import Control.Monad
import Control.Monad.Trans
import Control.DeepSeq

import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Lazy as LB
-- import qualified Data.ByteString as B


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



