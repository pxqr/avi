-- |
--   Copyright   :  (c) Sam Truzjan 2013
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
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
module Codec.AVI.RIFF
       ( BinarySize (..) -- TODO more to Internal.hs

         -- * Character codes
       , TwoCC
       , FourCC

         -- * JUNK
       , junkCC
       , HasJUNK (..)

         -- * Chunk
       , Chunk (..)
       , ppChunk
       , decodeChunk
       , encodeChunk

         -- * List
       , List  (..)
       , ppList
       , list
       , lookupList

         -- * Atom
       , Atom
       , AtomG (..)
       , ppAtom
       , atomType

         -- * RIFF
       , RIFF (..)
       , ppRIFF
       ) where

import Control.Applicative
import Control.Monad as M
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Convertible.Base
import Data.Foldable
import Data.List as L
import Data.String
import Data.Traversable
import Data.Typeable
import Text.PrettyPrint
import Foreign.Storable

--    All multibyte integers encoded in little-endian byte order,
--   though the API users shouldn't take care of it.

{-----------------------------------------------------------------------
  Size helpers
-----------------------------------------------------------------------}

class BinarySize a where
  binarySize :: a -> Int

  default binarySize :: Storable a => a -> Int
  binarySize = sizeOf

instance BinarySize Word16
instance BinarySize Word32
instance BinarySize Word64

instance BinarySize Int where
  binarySize _ = 4

instance BinarySize ByteString where
  {-# INLINE binarySize #-}
  binarySize bs = 4 + BS.length bs


getSized :: (Binary a, BinarySize a) => Int -> Get [a]
getSized n
  |   n <= 0  = pure []
  | otherwise = do
    x  <- get -- :: Get a
    xs <- getSized (n - binarySize x)
    pure (x : xs)

{-----------------------------------------------------------------------
  Two Character Code
-----------------------------------------------------------------------}

newtype TwoCC = TwoCC { twoCC :: Word16 }
                deriving (Eq, Ord, Typeable)

instance BinarySize TwoCC where
  {-# INLINE binarySize #-}
  binarySize _ = 2

instance IsString TwoCC where
  {-# INLINE fromString #-}
  fromString [a, b] = TwoCC $
    fromIntegral  (fromEnum a .&. 0xff) .|.
    fromIntegral ((fromEnum b .&. 0xff) `shiftL` 8)
  fromString _ = error "fromString: TwoCC should be 2 characters long"

instance Show TwoCC where
  show (TwoCC x) =
    [ toEnum $ fromIntegral  (x .&. 0xff)
    , toEnum $ fromIntegral ((x `shiftR` 8) .&. 0xff)
    ]

instance Binary TwoCC where
  get = TwoCC <$> getWord16le
  {-# INLINE get #-}

  put = putWord16le . twoCC
  {-# INLINE put #-}

{-----------------------------------------------------------------------
  Four Character Code
-----------------------------------------------------------------------}

-- | Compact representation for Four Character Code.
newtype FourCC = FourCC { fourCC :: Word32 }
                 deriving (Eq, Ord, Typeable)

instance BinarySize FourCC where
  {-# INLINE binarySize #-}
  binarySize _ = 4

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

checkCC :: FourCC -> Get ()
checkCC ex = do
  ac <- get
  unless (ac == ex) $ do
    fail $ "expected " ++ show ex ++ " but actual " ++ show ac

{-----------------------------------------------------------------------
  JUNK
-----------------------------------------------------------------------}

-- | Applications should ignore the content of JUNK chunks — they are
-- used for alignment.
junkCC :: FourCC
junkCC = "JUNK"

-- | 'Atom's which can have a JUNK('junkCC') chunk.
class HasJUNK a where
  -- | Remove all JUNK chunks from an RIFF atom.
  clean :: a -> a

{-----------------------------------------------------------------------
  Chunk
-----------------------------------------------------------------------}

-- TODO chunkData :: Lazy.ByteString ?
data Chunk = Chunk
  { chunkType :: {-# UNPACK #-} !FourCC
  , chunkData :: {-# UNPACK #-} !ByteString
  } deriving (Eq, Typeable)

instance BinarySize Chunk where
  {-# INLINE binarySize #-}
  binarySize Chunk {..} = binarySize chunkType + binarySize chunkData

-- | for Show instance only
data ChunkInfo = ChunkInfo
  { ckType :: {-# UNPACK #-} !FourCC
  , ckSize :: {-# UNPACK #-} !Int
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

  put Chunk {..} = do
    put chunkType
    put $ BS.length chunkData
    putByteString   chunkData

decodeChunk :: (Typeable a, Binary a) => FourCC -> Chunk -> ConvertResult a
decodeChunk ex c @ Chunk {..}
  | ex == chunkType = return $ decode $ LBS.fromChunks [chunkData]
  |    otherwise    = convError "unexpected chunk type" c

encodeChunk :: Binary a => FourCC -> a -> ConvertResult Chunk
encodeChunk ty x = return $ Chunk
  { chunkType = ty
  , chunkData = LBS.toStrict $ encode x
  }

{-----------------------------------------------------------------------
  List
-----------------------------------------------------------------------}

data List = List
  { listSize :: {-# UNPACK #-} !Int    -- TODO remove reduntant field in list
  , listType :: {-# UNPACK #-} !FourCC
  , children :: [Atom]
  } deriving (Show, Eq, Typeable)

instance HasJUNK List where
  clean List {..}
    = list listType $ L.filter ((==) junkCC . atomType) children

-- | Safe constructor.
list :: FourCC -> [Atom] -> List
list ty cs = List
  { listSize = L.sum $ L.map binarySize cs
  , listType = ty
  , children = cs
  }

-- | RIFF list identifier.
listCC :: FourCC
listCC = "LIST"

instance BinarySize List where
  binarySize List {..}
   = binarySize listCC
   + binarySize listSize
   + binarySize listType
   + listSize

getListBody :: Get List
getListBody = do
  payloadSize <- (\sz -> fromIntegral sz - binarySize listCC) <$> getWord32le
  List payloadSize <$> get <*> getSized payloadSize

putListBody :: List -> Put
putListBody List {..} = do
  put (listSize + binarySize listCC)
  put  listType
  M.mapM_ put children

instance Binary List where
  get = do
    checkCC listCC
    getListBody

  put lst = do
    put  listCC
    putListBody lst

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
               deriving ( Show, Eq, Typeable
                        , Functor, Foldable, Traversable
                        )

type Atom = AtomG Chunk

instance HasJUNK Atom where
  clean (AList  xs) = AList  (clean xs)
  clean (AChunk c ) = AChunk c

instance BinarySize Atom where
  binarySize (AChunk c) = binarySize c
  binarySize (AList  l) = binarySize l

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
  get = AList  <$> get
    <|> AChunk <$> get

  put (AList  l) = put l
  put (AChunk c) = put c

atomType :: Atom -> FourCC
atomType (AChunk Chunk {..}) = chunkType
atomType (AList  List  {..}) = listType

{-----------------------------------------------------------------------
  RIFF
-----------------------------------------------------------------------}

-- | Root atom.
newtype RIFF = RIFF List
               deriving (Show, Eq, Typeable)

-- | RIFF identifier.
riffCC :: FourCC
riffCC = "RIFF"

instance Binary RIFF where
  get = do
    checkCC riffCC
    RIFF <$> getListBody

  put (RIFF lst) = do
    put riffCC
    put lst

instance HasJUNK RIFF where
  clean (RIFF a) = RIFF (clean a)

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
ppAtom (AList  lst)   = ppList  lst

ppRIFF :: RIFF -> Doc
ppRIFF (RIFF lst) = ppList lst