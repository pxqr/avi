-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   Each stream contain mandatory /stream header chunk/, one per
--   stream list. The stream header chunk contains data-independent
--   information about the stream.
--
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.AVI.Stream.Header
       ( headerCC
       , StreamType (..)
       , Rect   (..)
       , Header (..)
       ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.String
import Data.Text as T
import Data.Typeable

import Codec.AVI.RIFF


{-----------------------------------------------------------------------
--  Stream Type
-----------------------------------------------------------------------}

-- | Types of the data stream could contain.
data StreamType
  = Audio -- ^ Audio stream.
  | Midi  -- ^ MIDI  stream.
  | Video -- ^ Video stream.
  | Text  -- ^ Text  stream (subtitles).
  | UnknownType Text -- ^ Not recognized stream type.
    deriving (Show, Read, Eq, Ord)

instance Binary StreamType where
  get = fourCCToST <$> get
    where
      fourCCToST :: FourCC -> StreamType
      fourCCToST "vids" = Audio
      fourCCToST "mids" = Midi
      fourCCToST "auds" = Video
      fourCCToST "txts" = Text
      fourCCToST t      = UnknownType $ T.pack $ show t

  put = put . stToFourCC
    where
      stToFourCC :: StreamType -> FourCC
      stToFourCC Audio = "vids"
      stToFourCC Midi  = "mids"
      stToFourCC Video = "auds"
      stToFourCC Text  = "txts"
      stToFourCC (UnknownType t) = fromString $ T.unpack t

{-----------------------------------------------------------------------
--  Rectangle
-----------------------------------------------------------------------}

-- | Rectangle used to specify relative location of video or text stream.
data Rect = Rect
  { left   :: {-# UNPACK #-} !Int -- ^ Left   offset in pixels;
  , top    :: {-# UNPACK #-} !Int -- ^ Top    offset in pixels;
  , right  :: {-# UNPACK #-} !Int -- ^ Right  offset in pixels;
  , bottom :: {-# UNPACK #-} !Int -- ^ Bottom offset in pixels.
  } deriving (Show, Typeable)

instance Binary Rect where
  get = Rect
    <$> (fromIntegral <$> getWord16le)
    <*> (fromIntegral <$> getWord16le)
    <*> (fromIntegral <$> getWord16le)
    <*> (fromIntegral <$> getWord16le)

  put Rect {..} = do
    putWord16le $ fromIntegral left
    putWord16le $ fromIntegral top
    putWord16le $ fromIntegral right
    putWord16le $ fromIntegral bottom

{-----------------------------------------------------------------------
--  Header
-----------------------------------------------------------------------}

-- | Stream header chunk identifier.
headerCC :: FourCC
headerCC = "strh"

-- | Stream header (or AVISTREAMHEADER) contains information about one
-- stream in an AVI file. For reference see:
--
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/dd318183(v=vs.85).aspx>
--
data Header = Header
  { -- | Type of the data contained in the stream.
    streamType          :: !StreamType

    -- | Codec to be used.
  , streamHandler       :: {-# UNPACK #-} !FourCC
  , streamFlags         :: {-# UNPACK #-} !Word32
    -- | Priorities can be used to set default audio stream for example.
  , streamPriority      :: {-# UNPACK #-} !Word16
    -- | Language tag.
  , streamLanguage      :: {-# UNPACK #-} !Word16
  , streamInitialFrames :: {-# UNPACK #-} !Word32

    -- | Used with 'streamRate' to specify the time scale that this
    -- stream will use. 'streamRate' / 'streamScale' =
    -- streamSamplesPerSec, specifies:
    --
    --   * for video stream — frame rate;
    --
    --   * for audio stream — sample rate.
    --
  , streamScale         :: {-# UNPACK #-} !Word32

    -- TODO streamSamplesPerSec :: Ratio Int ?

    -- | See 'streamScale'.
  , streamRate          :: {-# UNPACK #-} !Word32

    -- | Start time of stream indicates the number of silent frames.
  , streamStart         :: {-# UNPACK #-} !Word32
    -- | Length of this strem in 'streamRate' / 'streamScale' units.
  , streamLength        :: {-# UNPACK #-} !Word32

    -- | Size of block necessary to store blocks of that stream.
  , streamSuggestedBufferSize :: {-# UNPACK #-} !Word32

    -- | Quality of the stream typically passed to the compression
    -- software. If equals to -1, drivers use the default quality
    -- value.
  , streamQuality       :: {-# UNPACK #-} !Word32

    -- | Specifies the size of a single sample of data, equals to zero
    -- if the samples ca vary in size.
  , streamSampleSize    :: {-# UNPACK #-} !Word32

    -- | Destination rectablee for a text or video stream within the
    -- movie rectangle specified by the width and height members of
    -- the AVI main header structure.
  , streamFrame         :: !Rect
  } deriving (Show, Typeable)

instance Binary Header where
  get = Header
    <$> get
    <*> get
    <*> getWord32le
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le

    <*> getWord32le
    <*> getWord32le

    <*> getWord32le
    <*> getWord32le

    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> get

  put Header {..} = do
    put streamType
    put streamHandler

    putWord32le streamFlags
    putWord16le streamPriority
    putWord16le streamLanguage
    putWord32le streamInitialFrames

    putWord32le streamScale
    putWord32le streamRate

    putWord32le streamStart
    putWord32le streamLength

    putWord32le streamSuggestedBufferSize
    putWord32le streamQuality
    putWord32le streamSampleSize
    put streamFrame

instance Convertible Chunk Header where
  safeConvert = decodeChunk headerCC

instance Convertible Atom Header where
  safeConvert = convertVia (undefined :: Chunk)

-- should never fail
instance Convertible Header Chunk where
  safeConvert = encodeChunk headerCC

instance Convertible Header Atom where
  safeConvert = convertVia (undefined :: Chunk)
