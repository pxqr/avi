-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Each stream contain mandatory /stream format chunk/, one per
--   stream list. The stream format chunk describes the format of the
--   embedded data in the stream.
--
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.AVI.Stream.Format
       ( BitmapInfo  (..)
       , WaveFormatX (..)
       , Format      (..)
       , formatCC
       ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString as BS
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Typeable

import Codec.AVI.RIFF

-- | Identifier of stream format chunk.
formatCC :: FourCC
formatCC = "strf"

{-----------------------------------------------------------------------
-- BITMAPINFOHEADER
-----------------------------------------------------------------------}

-- | Format description used in video streams. For BITMAPINFOHEADER
-- reference see:
--
--   <http://msdn.microsoft.com/en-us/library/windows/desktop/dd318229(v=vs.85).aspx>
--
data BitmapInfo = BitmapInfo
  { -- | Number of bytes required by the structure.
    --   TODO remove reduntant field
    biSize   :: {-# UNPACK #-} !Word32

    -- | The width of the bitmap, in pixels.
  , biWidth  :: {-# UNPACK #-} !Word32

    -- | Height of the bitmap, in pixels.
  , biHeight :: {-# UNPACK #-} !Word32


    -- | Number of planes for the target device.
    --   For video stream this value should be equal to 1.
  , biPlanes      :: {-# UNPACK #-} !Word16
    -- | Number of bits per pixel.
  , biBitCount    :: {-# UNPACK #-} !Word16
    -- |
  , biCompression :: {-# UNPACK #-} !Word32

    -- | Size of the image in bytes. Can be equal to 0 for an
    -- uncompressed RGB bitmap.
  , biSizeImage     :: {-# UNPACK #-} !Word32
    -- | Horizontal resolution, in pixels per meter.
  , biXPelsPerMeter :: {-# UNPACK #-} !Word32
    -- | Vertical resolution, in pixels per meter.
  , biYPelsPerMeter :: {-# UNPACK #-} !Word32

    -- | Number of color indices in the color table that are actually
    -- used by the bitmap.
  , biClrUsed      :: {-# UNPACK #-} !Word32
    -- | Number of color indices considered important.
  , biClrImportant :: {-# UNPACK #-} !Word32
  } deriving (Show, Eq, Typeable)

-- TODO decode/encode color table

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
  safeConvert = decodeChunk formatCC

{-----------------------------------------------------------------------
-- WaveFormatX
-----------------------------------------------------------------------}

-- | Format description used in audio streams. For the header
-- reference see:
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/dd390970(v=vs.85).aspx>
--
data WaveFormatX = WaveFormatX
  { -- | Waveform-audio format type. TODO make enum
    formatTag      :: {-# UNPACK #-} !Word16
    -- | Number of channels: mono — 1, stereo — 2, etc.
  , channels       :: {-# UNPACK #-} !Word16

    -- | Sample rate: 11.025 kHz, 22.05 kHz, 44.1 kHz, etc.
  , samplesPerSec  :: {-# UNPACK #-} !Word32
    -- | Required average data-transfer rate.
  , avgBytesPerSec :: {-# UNPACK #-} !Word32

    -- | The block alignment is the minimum atomic unit of data for
    -- the formatTag format type.
  , blockAlign     :: {-# UNPACK #-} !Word16
    -- | Bits per sample for the 'formatTag' format type.
  , bitsPerSample  :: {-# UNPACK #-} !Word16

    -- | Size, in bytes, of extra format information appended to the
    -- end of the structure.
  , cbSize         :: {-# UNPACK #-} !Word16
  } deriving (Show, Eq, Typeable)

instance BinarySize WaveFormatX where
  binarySize WaveFormatX {..} = 18
  {-
    = binarySize formatTag
    + binarySize channels

    + binarySize samplesPerSec
    + binarySize avgBytesPerSec

    + binarySize blockAlign
    + binarySize bitsPerSample
    + binarySize cbSize
  -}

instance Binary WaveFormatX where
  get = WaveFormatX
    <$> getWord16le <*> getWord16le
    <*> getWord32le <*> getWord32le
    <*> getWord16le <*> getWord16le <*> getWord16le

  put WaveFormatX {..} = do
    putWord16le formatTag
    putWord16le channels

    putWord32le samplesPerSec
    putWord32le avgBytesPerSec

    putWord16le blockAlign
    putWord16le bitsPerSample
    putWord16le cbSize

instance Convertible Chunk WaveFormatX where
  safeConvert = decodeChunk formatCC

{-----------------------------------------------------------------------
-- WaveFormatX
-----------------------------------------------------------------------}

-- | Format description used in audio and video streams.
data Format
  = VideoFormat BitmapInfo
  | AudioFormat WaveFormatX
  deriving (Show, Eq, Typeable)

instance Convertible Chunk Format where
  safeConvert c @ Chunk {..} =
    case BS.length chunkData of
      40 -> VideoFormat <$> safeConvert c
      18 -> AudioFormat <$> safeConvert c
      n  -> convError ("unexpected chunk size" ++ show n) c

instance Convertible Atom Format where
  safeConvert = convertVia (undefined :: Chunk)