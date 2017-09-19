module TexBuilder.FileFilters
  ( Exts(..)
  , showExts
  , readExts
  , extFilter )
where

import qualified Data.List as L
import Control.Monad.State

import System.FilePath

data Exts = Exts [String]

showExts :: Exts -> String
showExts (Exts exts) = 
  L.intercalate "," exts

readExts :: String -> Maybe Exts
readExts [] = Nothing
readExts s = Just $ Exts parts
  where
    parts = evalState splitter (s,[])
    
    splitter = do
      (s,r) <- get
      case L.break (==',') s of
        (a,_:b) -> put (b,a:r) *> splitter
        (a,[]) -> pure (a:r)  

extFilter :: Exts -> FilePath -> Bool
extFilter (Exts exts) path =
  takeExtension path `elem` map ('.':) exts

