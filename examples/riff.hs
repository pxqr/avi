-- | This program write a RIFF file structure to stdout.
module Main (main) where

import Codec.AVI.RIFF
import Data.Binary
import Data.ByteString.Lazy as LBS
import System.Environment


main :: IO ()
main = do
  [filePath] <- getArgs
  bs <- LBS.readFile filePath
  print $ ppRIFF $ decode bs
