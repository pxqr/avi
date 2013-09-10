-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable
--
--   For AVI RIFF reference see:
--
--   <http://msdn.microsoft.com/en-us/library/windows/desktop/dd318189(v=vs.85).aspx>
--   <http://www.alexander-noe.com/video/documentation/avi.pdf>
--
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.AVI
       ( Stream (..)

       , Codec.AVI.Header (..)
       , AVI (..)
       ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.List as L
import Data.Typeable

import Codec.AVI.RIFF
import Codec.AVI.Stream

{-----------------------------------------------------------------------
  AVI
-----------------------------------------------------------------------}

avi :: FourCC
avi = "AVI "

avix :: FourCC
avix = "AVIX"

data Flag = HasIndex
          | MustUseIndex
          | IsInterleaved
          | WasCaptureFile
          | Copyrighted
          | TrustCKType
            deriving (Show, Read, Eq, Ord, Enum)

data Header = Header
  { microSecPerFrame    :: {-# UNPACK #-} !Word32
  , maxBytesPerSec      :: {-# UNPACK #-} !Word32
  , paddingGranularity  :: {-# UNPACK #-} !Word32

  , flags               :: {-# UNPACK #-} !Word32
  , totalFrames         :: {-# UNPACK #-} !Word32
  , initialFrames       :: {-# UNPACK #-} !Word32

    -- | Number of streams in the file.
  , streamCount         :: {-# UNPACK #-} !Word32
  , suggestedBufferSize :: {-# UNPACK #-} !Word32

    -- | Width of video stream.
  , width               :: {-# UNPACK #-} !Word32

    -- | Height of video stream.
  , height              :: {-# UNPACK #-} !Word32

  , reserved            :: {-# UNPACK #-} !Word32
  } deriving (Show, Typeable)

instance Binary Codec.AVI.Header where
  get = Codec.AVI.Header
    <$> getWord32le
    <*> getWord32le
    <*> getWord32le

    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

    <*> getWord32le
    <*> getWord32le

    <*> getWord32le

  put = undefined

instance Convertible Chunk Codec.AVI.Header where
  safeConvert ck @ Chunk {..}
    | chunkType == "avih" = return $ decode $ LBS.fromChunks [chunkData]
    |       otherwise     = convError ("unexpected" ++ show chunkType) ck

instance Convertible Atom Codec.AVI.Header where
  safeConvert = convertVia (undefined :: Chunk)

data Idx1 = Idx1
  { chunkId     :: {-# UNPACK #-} !Word32
  , idx1Flags   :: {-# UNPACK #-} !Word32
  , chunkOffset :: {-# UNPACK #-} !Word32
  , chunkLength :: {-# UNPACK #-} !Word32
  } deriving Show

data AVI = AVI
  { header  :: Codec.AVI.Header
  , streams :: [Stream]
  } deriving (Show, Typeable)

instance Convertible RIFF AVI where
  safeConvert xs @ (RIFF List {..})
    | listType /= avi
    = convError ("unexpected list type: " ++ show listType) xs
    |    otherwise    = AVI
      <$> (safeConvert =<< lookupList "avih" hdrl)
      <*> (mapM safeConvert $ L.filter ((streamCC ==) . atomType) hdrl)
    where
      Right (AList (List {children = hdrl})) = lookupList "hdrl" children

instance Binary AVI where
  get = do
    xs <- get
    either (fail . prettyConvertError) return $ safeConvert (xs :: RIFF)

  put = undefined
