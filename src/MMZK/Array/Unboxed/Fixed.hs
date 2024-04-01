{-# LANGUAGE StandaloneDeriving #-}

module MMZK.Array.Unboxed.Fixed where

import           Data.Array.Base
import           Data.Array.IArray
import           Data.Proxy
import           GHC.TypeLits

-- | A fixed-length, 'Int'-indexed array. The length is known at compile time.
--
-- The 'readArr' function allows us to read from a compile-time known index
-- without runtime bounds checking.
type UArrayFixed len e = UArrayFixed' len Int e

newtype UArrayFixed' (len :: Nat) ix e = UArrayFixed (UArray ix e)

deriving newtype instance (Eq e, Ix ix, IArray UArray e) 
  => Eq (UArrayFixed' len ix e)

deriving newtype instance (Ord e, Ix ix, IArray UArray e)
  => Ord (UArrayFixed' len ix e)

instance (Show e, Show ix, Ix ix, IArray UArray e, KnownNat len)
  => Show (UArrayFixed' len ix e) where
    showsPrec :: Int -> UArrayFixed' len ix e -> ShowS
    showsPrec d (UArrayFixed arr) = ("length " ++)
                                  . showsPrec d (natVal (Proxy @len))
                                 . (": " ++) . showsPrec d arr
    {-# INLINE showsPrec #-}

instance (IArray UArray e, KnownNat len) => IArray (UArrayFixed' len) e where
  bounds :: Ix ix => UArrayFixed' len ix e -> (ix, ix)
  bounds (UArrayFixed arr) = bounds arr
  {-# INLINE bounds #-}

  numElements :: Ix ix => UArrayFixed' len ix e -> Int
  numElements _ = fromIntegral (natVal (Proxy @len))
  {-# INLINE numElements #-}

  unsafeArray :: Ix ix => (ix, ix) -> [(Int, e)] -> UArrayFixed' len ix e
  unsafeArray = (UArrayFixed .) . unsafeArray
  {-# INLINE unsafeArray #-}

  unsafeAt :: Ix ix => UArrayFixed' len ix e -> Int -> e
  unsafeAt (UArrayFixed arr) = unsafeAt arr
  {-# INLINE unsafeAt #-}
