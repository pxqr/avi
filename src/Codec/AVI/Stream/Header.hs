{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.AVI.Stream.Header
       ( headerCC
       , Rect   (..)
       , Header (..)
       ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.String
import Data.Text as T
import Data.Typeable

import Codec.AVI.RIFF


{-----------------------------------------------------------------------
--  Stream Type
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

{-----------------------------------------------------------------------
--  Rectangle
-----------------------------------------------------------------------}

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



{-----------------------------------------------------------------------
--  Header
-----------------------------------------------------------------------}

headerCC :: FourCC
headerCC = "strh"

data Header = Header
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

-- |
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/dd318183(v=vs.85).aspx>
--
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

  put = undefined

instance Convertible Chunk Header where
  safeConvert = decodeChunk headerCC

instance Convertible Atom Header where
  safeConvert = convertVia (undefined :: Chunk)
