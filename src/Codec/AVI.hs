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
import Data.Binary.Put
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.List as L
import Data.Maybe
import Data.Typeable

import Codec.AVI.RIFF
import Codec.AVI.Stream

{-----------------------------------------------------------------------
  AVI Header
-----------------------------------------------------------------------}

-- | AVI header chunk identifier.
aviHeaderCC :: FourCC
aviHeaderCC = "avih"

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
  safeConvert = decodeChunk aviHeaderCC

instance Convertible Atom Codec.AVI.Header where
  safeConvert = convertVia (undefined :: Chunk)

instance Convertible Codec.AVI.Header Chunk where
  safeConvert = encodeChunk aviHeaderCC

instance Convertible Codec.AVI.Header Atom where
  safeConvert = convertVia (undefined :: Chunk)

{-----------------------------------------------------------------------
  AVIOLDINDEX
-----------------------------------------------------------------------}

data ContentType
  = UncompressedVideo
  | CompressedVideo
  | PaletteChange
  | Audio
  | UnknownContent TwoCC
    deriving (Show, Eq, Ord, Typeable)

instance Binary ContentType where
  get = tccToCTy <$> get
    where
      tccToCTy :: TwoCC -> ContentType
      tccToCTy "db" = UncompressedVideo
      tccToCTy "dc" = CompressedVideo
      tccToCTy "pc" = PaletteChange
      tccToCTy "wb" = Audio
      tccToCTy  cc  = UnknownContent cc

  put = put . cTyToTcc
    where
      cTyToTcc :: ContentType -> TwoCC
      cTyToTcc  UncompressedVideo  = "db"
      cTyToTcc  CompressedVideo    = "dc"
      cTyToTcc  PaletteChange      = "pc"
      cTyToTcc  Audio              = "wb"
      cTyToTcc (UnknownContent cc) = cc

-- | Index version 1 identifier.
idx1CC :: FourCC
idx1CC = "idx1"

-- | http://msdn.microsoft.com/en-us/library/windows/desktop/dd318181(v=vs.85).aspx
data Idx1 = Idx1
  { -- |
    streamIx    :: {-# UNPACK #-} !Word16
  , streamType  :: !ContentType

    -- | TODO
  , idx1Flags   :: {-# UNPACK #-} !Word32

    -- | Offset from the start of the movi list, in bytes.
  , chunkOffset :: {-# UNPACK #-} !Word32

    -- | Size of the data chunk starting from 'chunkOffset', in bytes.
  , chunkLength :: {-# UNPACK #-} !Word32
  } deriving (Show, Eq, Typeable)

instance Binary Idx1 where
  get = Idx1
     <$> getWord16le
     <*> get
     <*> getWord32le
     <*> getWord32le
     <*> getWord32le

  put Idx1 {..} = do
    putWord16le streamIx
    put         streamType
    putWord32le idx1Flags
    putWord32le chunkOffset
    putWord32le chunkLength

instance Convertible Chunk Idx1 where
  safeConvert = decodeChunk idx1CC

instance Convertible Atom Idx1 where
  safeConvert = convertVia (undefined :: Chunk)

instance Convertible Idx1 Chunk where
  safeConvert = encodeChunk idx1CC

instance Convertible Idx1 Atom where
  safeConvert = convertVia (undefined :: Chunk)

{-----------------------------------------------------------------------
  AVI
-----------------------------------------------------------------------}

-- | AVI LIST identifier.
aviCC :: FourCC
aviCC = "AVI "

avix :: FourCC
avix = "AVIX"

-- | Identifier of stream LIST.
headerListCC :: FourCC
headerListCC = "hdrl"

data AVI = AVI
  { header  :: Codec.AVI.Header
  , streams :: [Stream]
  , index   :: Maybe Idx1
  } deriving (Show, Typeable)

convMaybe :: ConvertResult a -> ConvertResult (Maybe a)
convMaybe = pure . either (const Nothing) Just

instance Convertible RIFF AVI where
  safeConvert xs @ (RIFF List {..})
    | listType /= aviCC
    = convError ("unexpected list type: " ++ show listType) xs
    |    otherwise    = AVI
      <$> (safeConvert =<< lookupList aviHeaderCC hdrl)
      <*> (mapM safeConvert $ L.filter ((streamCC ==) . atomType) hdrl)
      <*> convMaybe (safeConvert =<< lookupList idx1CC children)
    where
      Right (AList (List {children = hdrl})) = lookupList headerListCC children

instance Convertible AVI RIFF where
  safeConvert AVI {..} = pure $ RIFF $ list aviCC xs
    where
      xs =
        [ convert header
        , convert $ list headerListCC $ L.map convert streams
        ] ++ maybeToList (convert <$> index)

instance Binary AVI where
  get = do
    xs <- get
    either (fail . prettyConvertError) return $ safeConvert (xs :: RIFF)

  put = undefined
