-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.AVI.Stream.Format
       ( BitmapInfo (..)
       ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy as LBS
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Typeable

import Codec.AVI.RIFF


-- | BitmapInfo is used for stream format for video stream. For
-- BITMAPINFOHEADER reference see:
--
--   <http://msdn.microsoft.com/en-us/library/windows/desktop/dd318229(v=vs.85).aspx>
--
data BitmapInfo = BitmapInfo
  { biSize   :: {-# UNPACK #-} !Word32
  , biWidth  :: {-# UNPACK #-} !Word32
  , biHeight :: {-# UNPACK #-} !Word32

  , biPlanes      :: {-# UNPACK #-} !Word16
  , biBitCount    :: {-# UNPACK #-} !Word16
  , biCompression :: {-# UNPACK #-} !Word32

  , biSizeImage     :: {-# UNPACK #-} !Word32
  , biXPelsPerMeter :: {-# UNPACK #-} !Word32
  , biYPelsPerMeter :: {-# UNPACK #-} !Word32

  , biClrUsed      :: {-# UNPACK #-} !Word32
  , biClrImportant :: {-# UNPACK #-} !Word32
  } deriving (Show, Eq, Typeable)

instance BinarySize BitmapInfo where
  {-# INLINE binarySize #-}
  binarySize BitmapInfo {..}
    = binarySize biSize
    + binarySize biWidth
    + binarySize biHeight

    + binarySize biPlanes
    + binarySize biBitCount
    + binarySize biCompression

    + binarySize biSizeImage
    + binarySize biXPelsPerMeter
    + binarySize biYPelsPerMeter

    + binarySize biClrUsed
    + binarySize biClrImportant

instance Binary BitmapInfo where
  get = BitmapInfo
    <$> getWord32le
    <*> getWord32le
    <*> getWord32le

    <*> getWord16le
    <*> getWord16le
    <*> getWord32le

    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

    <*> getWord32le
    <*> getWord32le

  put BitmapInfo {..} = do
    putWord32le biSize
    putWord32le biWidth
    putWord32le biHeight

    putWord16le biPlanes
    putWord16le biBitCount
    putWord32le biCompression

    putWord32le biSizeImage
    putWord32le biXPelsPerMeter
    putWord32le biXPelsPerMeter

    putWord32le biClrUsed
    putWord32le biClrImportant


instance Convertible Chunk BitmapInfo where
  safeConvert Chunk {..} = return $ decode $ LBS.fromChunks [chunkData]

instance Convertible Atom BitmapInfo where
  safeConvert = convertVia (undefined :: Chunk)

-- |
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/dd390970(v=vs.85).aspx>
--
data WaveFormatX = WaveFormatX
  { formatTag      :: {-# UNPACK #-} !Word16
  , channels       :: {-# UNPACK #-} !Word16

  , samplesPerSec  :: {-# UNPACK #-} !Word32
  , avgBytesPerSec :: {-# UNPACK #-} !Word32

  , blockAlign     :: {-# UNPACK #-} !Word16
  , bitsPerSample  :: {-# UNPACK #-} !Word16
  , cbSize         :: {-# UNPACK #-} !Word16
  } deriving (Show, Eq, Typeable)

instance BinarySize WaveFormatX where
  binarySize WaveFormatX {..}
    = binarySize formatTag
    + binarySize channels

    + binarySize samplesPerSec
    + binarySize avgBytesPerSec

    + binarySize blockAlign
    + binarySize bitsPerSample
    + binarySize cbSize