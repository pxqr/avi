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

streamCC :: FourCC
streamCC = "strl"

data Stream = Stream
  { streamHeader :: !Stream.Header
  , streamFormat :: !Stream.Format
  , streamData   :: !(Maybe StreamData)
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
