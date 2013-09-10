-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  non-portable (depends on Codec.AVI.RIFF)
--
--   For stream header reference see:
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/dd318189(v=vs.85).aspx#AVI_STREAM_HEADERS>
--
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.AVI.Stream
       ( streamCC
       , Stream (..)
       ) where

import Control.Applicative
import Data.Binary
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Text as T
import Data.Typeable

import Codec.AVI.RIFF
import Codec.AVI.Stream.Header as Stream
import Codec.AVI.Stream.Format as Stream


type StreamName   = Text
type StreamIndex  = Chunk
type StreamData   = Chunk

-- | Stream list identifier.
streamCC :: FourCC
streamCC = "strl"

-- TODO s/Stream/StreamInfo ?

-- |
data Stream = Stream
  { -- | Data independent stream information.
    streamHeader :: !Stream.Header

    -- | Format of the stream data.
  , streamFormat :: !Stream.Format

    -- | Format of content the stream data chunk is codec driver dependent.
    --   Drivers use this information for configuration.
  , streamData   :: !(Maybe StreamData)

    -- | Optional stream name used to display to user.
  , streamName   :: !(Maybe StreamName)

  , streamIndex  :: !(Maybe StreamIndex)
  } deriving (Show, Typeable)

instance Convertible List Stream where
  safeConvert xs @ List {..}
    | listType /= streamCC = convError "unexpected list four CC" xs
    |       otherwise      = Stream
      <$> (safeConvert =<< lookupList headerCC children)
      <*> (safeConvert =<< lookupList formatCC children)
      <*> pure Nothing
      <*> pure Nothing
      <*> pure Nothing

instance Convertible Atom Stream where
  safeConvert = convertVia (undefined :: List)

instance Convertible Stream List where
  safeConvert Stream {..} = pure $ list streamCC
    [ convert streamHeader
    , convert streamFormat
    ]

instance Convertible Stream Atom where
  safeConvert = convertVia (undefined :: List)

instance Binary Stream where
  put s = put (convert s :: List)
  get = do
    lst <- get :: Get List
    either (fail . prettyConvertError) return $ safeConvert lst
