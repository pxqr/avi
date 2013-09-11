-- | This program remove all JUNK chunks from a RIFF file.
module Main (main) where

import Codec.AVI.RIFF
import Data.Binary
import Data.ByteString.Lazy as LBS
import System.Environment


main :: IO ()
main = do
  [src, dst] <- getArgs
  bs <- LBS.readFile src
  let bs' = encode $ clean (decode bs :: RIFF)
  LBS.writeFile dst bs'