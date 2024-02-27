{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a class 'MMZKRead' for reading values from 'String's.
--
-- In particular, it provides alternative implementations for 'readEither' and
-- 'readMaybe' for fixe-size 'Word' and 'Int' types, which are not only faster
-- than the default implementations but also correct. The default
-- implementations of 'readEither' and 'readMaybe' for 'Word' and 'Int' types
-- are incorrect, because they do not check for overflow.
--
-- The 'rdEither' and 'rdMaybe' functions also work for all instances of the
-- 'Read' class using the plain implementations of 'readEither' and 'readMaybe'.
module MMZK.Read where

import           MMZK.Maybe
import           MMZK.Read.Internal
import qualified Text.Read

-- | A class for reading values from 'String's. Alternative implementations for
-- fix-sized 'Word' and 'Int' types are provided.
class MMZKRead a where
  {-# MINIMAL rdEither | rdMaybe #-}

  -- | The counterpart of 'Text.Read.readEither'.
  rdEither :: String -> Either String a
  rdEither = m2e "MMZK.Read.rdEither: parse error" . rdMaybe
  {-# INLINE rdEither #-}

  -- | The counterpart of 'Text.Read.readMaybe'.
  rdMaybe :: String -> Maybe a
  rdMaybe = e2m . rdEither
  {-# INLINE rdMaybe #-}

  -- | The counterpart of 'Text.Read.read'.
  rd :: String -> a
  rd str = case rdEither str of
    Left e  -> error e
    Right x -> x
  {-# INLINE rd #-}

instance {-# OVERLAPPABLE #-} Read a => MMZKRead a where
  rdEither :: Read a => String -> Either String a
  rdEither = Text.Read.readEither
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Word8 where
  rdEither :: String -> Either String Word8
  rdEither = readWord8Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Word16 where
  rdEither :: String -> Either String Word16
  rdEither = readWord16Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Word32 where
  rdEither :: String -> Either String Word32
  rdEither = readWord32Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Word64 where
  rdEither :: String -> Either String Word64
  rdEither = readWord64Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Word where
  rdEither :: String -> Either String Word
  rdEither = readWordWheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Int8 where
  rdEither :: String -> Either String Int8
  rdEither = readInt8Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Int16 where
  rdEither :: String -> Either String Int16
  rdEither = readInt16Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Int32 where
  rdEither :: String -> Either String Int32
  rdEither = readInt32Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Int64 where
  rdEither :: String -> Either String Int64
  rdEither = readInt64Wheel'
  {-# INLINE rdEither #-}

instance {-# OVERLAPPING #-} MMZKRead Int where
  rdEither :: String -> Either String Int
  rdEither = readIntWheel'
  {-# INLINE rdEither #-}

-- | Similar to 'MMZKRead', but only for fix-sized 'Word' and 'Int' types.
class ReadFixedSizeNum a where
  {-# MINIMAL readNumEither | readNumMaybe #-}

  readNumEither :: String -> Either String a
  readNumEither = m2e "MMZK.Read.readNumEither: parse error" . readNumMaybe
  {-# INLINE readNumEither #-}

  readNumMaybe :: String -> Maybe a
  readNumMaybe = e2m . readNumEither
  {-# INLINE readNumMaybe #-}

instance ReadFixedSizeNum Word8 where
  readNumEither :: String -> Either String Word8
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Word16 where
  readNumEither :: String -> Either String Word16
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Word32 where
  readNumEither :: String -> Either String Word32
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Word64 where
  readNumEither :: String -> Either String Word64
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Word where
  readNumEither :: String -> Either String Word
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Int8 where
  readNumEither :: String -> Either String Int8
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Int16 where
  readNumEither :: String -> Either String Int16
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Int32 where
  readNumEither :: String -> Either String Int32
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Int64 where
  readNumEither :: String -> Either String Int64
  readNumEither = rdEither
  {-# INLINE readNumEither #-}

instance ReadFixedSizeNum Int where
  readNumEither :: String -> Either String Int
  readNumEither = rdEither
  {-# INLINE readNumEither #-}
