{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

#include "MachDeps.h"

-- | A helper module that provides an injective function from a type to 'Int'.
--
-- Its original use case is for extending "Int"-indexed data structures such as
-- "Data.IntSet" to work with types "smaller" than "Int" (e.g. "Int8", "Int16").
module MMZK.Int.Injection
  (Intable(..)
  ) where

import           Data.Int
import           Data.Word
import           Unsafe.Coerce (unsafeCoerce)

-- | A type class that provides an injective function from a type to 'Int'.
--
-- Its original use case is for extending "Int"-indexed data structures such as
-- "Data.IntSet" to work with types "smaller" than "Int" (e.g. "Int8", "Int16").
--
-- Note that the function is not necessarily the identity function. For
-- instance, the instance for "Word8" uses 'unsafeCoerce' to convert a "Word8"
-- value to an "Int" value, effectively reinterpreting the most significant bit
-- as the sign bit.
class Intable a where
  convertToInt :: a -> Int

instance Intable Int where
  convertToInt :: Int -> Int
  convertToInt = id
  {-# INLINE convertToInt #-}

instance Intable Word where
  convertToInt :: Word -> Int
  convertToInt = unsafeCoerce
  {-# INLINE convertToInt #-}

instance Intable Bool where
  convertToInt :: Bool -> Int
  convertToInt = fromEnum
  {-# INLINE convertToInt #-}

instance Intable () where
  convertToInt :: () -> Int
  convertToInt _ = 0
  {-# INLINE convertToInt #-}

#if WORD_SIZE_IN_BITS >= 8
instance Intable Int8 where
  convertToInt :: Int8 -> Int
  convertToInt = fromIntegral
  {-# INLINE convertToInt #-}

instance Intable Word8 where
  convertToInt :: Word8 -> Int
  convertToInt = convertToInt @Int8 . unsafeCoerce
  {-# INLINE convertToInt #-}
#endif

#if WORD_SIZE_IN_BITS >= 16
instance Intable Int16 where
  convertToInt :: Int16 -> Int
  convertToInt = fromIntegral
  {-# INLINE convertToInt #-}

instance Intable Word16 where
  convertToInt :: Word16 -> Int
  convertToInt = convertToInt @Int16 . unsafeCoerce
  {-# INLINE convertToInt #-}
#endif

#if WORD_SIZE_IN_BITS >= 32
instance Intable Int32 where
  convertToInt :: Int32 -> Int
  convertToInt = fromIntegral
  {-# INLINE convertToInt #-}

instance Intable Word32 where
  convertToInt :: Word32 -> Int
  convertToInt = convertToInt @Int32 . unsafeCoerce
  {-# INLINE convertToInt #-}
#endif

#if WORD_SIZE_IN_BITS >= 64
instance Intable Int64 where
  convertToInt :: Int64 -> Int
  convertToInt = fromIntegral
  {-# INLINE convertToInt #-}

instance Intable Word64 where
  convertToInt :: Word64 -> Int
  convertToInt = convertToInt @Int64 . unsafeCoerce
  {-# INLINE convertToInt #-}
#endif
