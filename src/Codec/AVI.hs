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
       ( Rect (..)
       , StreamHeader (..)
       , StreamFormat
       , StreamData
       , StreamName
       , StreamIndex
       , Stream (..)

       , Header (..)
       , AVI (..)
       ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.String
import Data.Text as T
import Data.Typeable

import Codec.AVI.RIFF

{-----------------------------------------------------------------------
  Stream
-----------------------------------------------------------------------}

data StreamType
  = Audio
  | Midi
  | Video
  | Text
  | UnknownType Text
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

data Rect = Rect
  { left   :: {-# UNPACK #-} !Int
  , top    :: {-# UNPACK #-} !Int
  , right  :: {-# UNPACK #-} !Int
  , bottom :: {-# UNPACK #-} !Int
  } deriving (Show, Typeable)

instance Binary Rect where
  get = Rect
    <$> (fromIntegral <$> getWord16le)
    <*> (fromIntegral <$> getWord16le)
    <*> (fromIntegral <$> getWord16le)
    <*> (fromIntegral <$> getWord16le)

  put = undefined

data StreamHeader = StreamHeader
  { streamType          :: !StreamType

    -- | Codec to be used.
  , streamHandler       :: {-# UNPACK #-} !FourCC
    -- |
  , streamFlags         :: {-# UNPACK #-} !Word32
  , streamPriority      :: {-# UNPACK #-} !Word16
  , streamLanguage      :: {-# UNPACK #-} !Word16
  , streamInitialFrames :: {-# UNPACK #-} !Word32
    -- |
  , streamScale         :: {-# UNPACK #-} !Word32
  , streamRate          :: {-# UNPACK #-} !Word32

    -- | Start time of stream indicates the number of silent frames.
  , streamStart         :: {-# UNPACK #-} !Word32
  , streamLength        :: {-# UNPACK #-} !Word32

    -- | Size of block necessary to store blocks of that stream.
  , streamSuggestedBufferSize :: {-# UNPACK #-} !Word32
    -- | Quality of the stream.
  , streamQuality       :: {-# UNPACK #-} !Word32
  , streamSampleSize    :: {-# UNPACK #-} !Word32
  , streamFrame         :: !Rect
  } deriving (Show, Typeable)

instance Binary StreamHeader where
  get = StreamHeader
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

  put = undefined

instance Convertible Chunk StreamHeader where
  safeConvert c @ Chunk {..}
    | chunkType == "strh" = return $ decode $ LBS.fromChunks [chunkData]
    |       otherwise     = convError "chunk type mismatch" c

instance Convertible Atom StreamHeader where
  safeConvert = convertVia (undefined :: Chunk)

type StreamName   = Text
type StreamFormat = Chunk
type StreamIndex  = Chunk
type StreamData   = Chunk

data Stream = Stream
  { streamHeader :: !StreamHeader
  , streamFormat :: !StreamFormat
  , streamData   :: !(Maybe StreamData)
  , streamName   :: !(Maybe StreamName)
  , streamIndex  :: !(Maybe StreamIndex)
  } deriving (Show, Typeable)

instance Convertible List Stream where
  safeConvert xs @ List {..}
    | listType /= "strl" = convError "not expected list four CC" xs
    |      otherwise     = Stream
      <$> (safeConvert =<< lookupList "strh" children)
      <*> (safeConvert =<< lookupList "strf" children)
      <*> pure Nothing
      <*> pure Nothing
      <*> pure Nothing

instance Convertible Atom Stream where
  safeConvert = convertVia (undefined :: List)

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

instance Binary Header where
  get = Header
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

instance Convertible Chunk Header where
  safeConvert ck @ Chunk {..}
    | chunkType == "avih" = return $ decode $ LBS.fromChunks [chunkData]
    |       otherwise     = convError ("unexpected" ++ show chunkType) ck

instance Convertible Atom Header where
  safeConvert = convertVia (undefined :: Chunk)

data AVI = AVI
  { header  :: Header
  , streams :: Stream
  } deriving (Show, Typeable)

instance Convertible RIFF AVI where
  safeConvert xs @ (RIFF List {..})
    | listType /= avi
    = convError ("unexpected list type: " ++ show listType) xs
    |    otherwise    = AVI
      <$> (safeConvert =<< lookupList "avih" hdrl)
      <*> (safeConvert =<< lookupList "strl" hdrl)
    where
      Right (AList (List {children = hdrl})) = lookupList "hdrl" children

instance Binary AVI where
  get = do
    xs <- get
    either (fail . prettyConvertError) return $ safeConvert (xs :: RIFF)

  put = undefined
