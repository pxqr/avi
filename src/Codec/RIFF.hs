-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  non-portable (see IsString FourCC)
--
--   Resource Interchange File Format (RIFF) - is a univeral file
--   container format primarily used for multimedia files. Every RIFF
--   file consist of two kinds of 'Atom's:
--
--     * 'Chunk' — raw <type> + <size> prefixed chunk of data. RIFF
--     format do not expose any restriction for chunk payload, but
--     usually chunks contain video, audio, text or some header
--     metadata.
--
--     * 'List' of 'Atom's.
--
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.RIFF
       ( FourCC

       , Chunk (..)
       , ppChunk

       , List  (..)
       , ppList
       , lookupList

       , Atom
       , AtomG (..)
       , ppAtom
       , atomType

       , RIFF
       ) where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString as BS
import Data.Convertible.Base
import Data.Foldable
import Data.List as L
import Data.String
import Data.Traversable
import Data.Typeable
import Text.PrettyPrint

--    All multibyte integers encoded in little-endian byte order,
--   though the API users shouldn't take care of it.

{-----------------------------------------------------------------------
  Four Character Code
-----------------------------------------------------------------------}

-- | Compact representation for Four Character Code.
newtype FourCC = FourCC { fourCC :: Word32 }
                 deriving (Eq, Ord, Typeable)

instance IsString FourCC where
  fromString [a, b, c, d] = FourCC $
    fromIntegral ( fromEnum a .&. 0xff)              .|.
    fromIntegral ((fromEnum b .&. 0xff) `shiftL` 8)  .|.
    fromIntegral ((fromEnum c .&. 0xff) `shiftL` 16) .|.
    fromIntegral ((fromEnum d .&. 0xff) `shiftL` 24)

  fromString _   = error "fromString: FourCC should be 4 character long"
  {-# INLINE fromString #-}

instance Show FourCC where
  show (FourCC x) =
    [ toEnum $ fromIntegral (x .&. 0xff)
    , toEnum $ fromIntegral ((x `shiftR` 8)  .&. 0xff)
    , toEnum $ fromIntegral ((x `shiftR` 16) .&. 0xff)
    , toEnum $ fromIntegral ((x `shiftR` 24) .&. 0xff)
    ]

instance Binary FourCC where
  get = FourCC <$> getWord32le
  {-# INLINE get #-}
  put = putWord32le . fourCC
  {-# INLINE put #-}

{-----------------------------------------------------------------------
  Chunk
-----------------------------------------------------------------------}

data Chunk = Chunk
  { chunkType :: {-# UNPACK #-} !FourCC
  , chunkData :: {-# UNPACK #-} !ByteString
  } deriving Typeable

-- | for Show instance only
data ChunkInfo = ChunkInfo
  { ckType :: {-# UNPACK #-} !FourCC
  , ckSize   :: {-# UNPACK #-} !Int
  } deriving Show

chunkInfo :: Chunk -> ChunkInfo
chunkInfo Chunk {..} = ChunkInfo
  { ckType = chunkType
  , ckSize = BS.length chunkData
  }

instance Show Chunk where
  showsPrec i = showsPrec i . chunkInfo

instance Binary Chunk where
  get = Chunk
    <$> get
    <*> (getWord32le >>= getByteString . fromIntegral)

  put = undefined

{-----------------------------------------------------------------------
  List
-----------------------------------------------------------------------}

data List = List
  { listSize :: {-# UNPACK #-} !Int
  , listType :: {-# UNPACK #-} !FourCC
  , children :: [Atom]
  } deriving (Show, Typeable)

instance Binary List where
  get = do
    get :: Get FourCC
    List <$> (fromIntegral <$> getWord32le)
         <*> get
         <*> many get

  put = undefined

lookupList :: FourCC -> [Atom] -> ConvertResult Atom
lookupList ty (a : as)
  | atomType a == ty = return a
  |     otherwise    = lookupList ty as
lookupList ty e      = convError ("could not lookup: " ++ show ty) e

{-----------------------------------------------------------------------
  Atom
-----------------------------------------------------------------------}

data AtomG a = AChunk !a
             | AList  !List
               deriving (Show, Functor, Foldable, Traversable, Typeable)

type Atom = AtomG Chunk

instance Convertible List Atom where
  {-# INLINE safeConvert #-}
  safeConvert = return . AList

instance Convertible Chunk Atom where
  {-# INLINE safeConvert #-}
  safeConvert = return . AChunk

instance Convertible Atom List where
  {-# INLINE safeConvert #-}
  safeConvert a @ (AChunk _) = convError "list expected" a
  safeConvert     (AList  l) = return l

instance Convertible Atom Chunk where
  {-# INLINE safeConvert #-}
  safeConvert a @ (AList  _) = convError "chunk expected" a
  safeConvert     (AChunk c) = return c

instance Binary (AtomG Chunk) where
  get = do
    bs <- lookAhead get
    if bs == "RIFF" || bs == ("LIST" :: FourCC)
      then AList  <$> get
      else AChunk <$> get

  put = undefined

atomType :: Atom -> FourCC
atomType (AChunk Chunk {..}) = chunkType
atomType (AList  List  {..}) = listType

-- | Root atom.
type RIFF = List

-- TODO filter JUNK chunks


{-----------------------------------------------------------------------
  Pretty printing
-----------------------------------------------------------------------}

-- | Format a 'Chunk' in human readable form omitting binary data.
ppChunk :: Chunk -> Doc
ppChunk Chunk {..}
  = text (show chunkType) <+> int (BS.length chunkData)

-- | Format a 'List' in human readable form omitting binary data.
ppList :: List -> Doc
ppList List {..}
  = text (show listType) <+> int listSize $+$
      nest 2 (vcat (L.map ppAtom children))

-- | Format an 'Atom' in human readable form omitting binary data.
ppAtom :: Atom -> Doc
ppAtom (AChunk chunk) = ppChunk chunk
ppAtom (AList  list)  = ppList  list
