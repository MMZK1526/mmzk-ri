{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module MMZK.Array.Unboxed.Fixed.Internal where

import           Data.Array.Base
import           Data.Array.IArray
import           Data.Proxy
import           Data.Type.Ord
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

-- | Read an element from a compile-time known index without runtime bounds
-- checking.
readArr :: forall loc len e
         . (IArray UArray e, KnownNat loc, KnownNat len, loc >= 0, loc < len)
        => UArrayFixed len e -> e
readArr = readArr' @loc
{-# INLINE readArr #-}

readArr' :: forall loc len e ix
          . ( IArray UArray e
            , KnownNat loc
            , KnownNat len
            , Ix ix
            , loc >= 0
            , loc < len )
         => UArrayFixed' len ix e -> e
readArr' arr = unsafeAt arr (fromIntegral (natVal (Proxy @loc)))
{-# INLINE readArr' #-}

-- | Convert a fixed-length array to a normal array, effectively erasing the
-- compile-time known length.
eraseLen :: Ix ix => IArray UArray e => UArrayFixed len e -> UArray Int e
eraseLen (UArrayFixed arr) = arr
{-# INLINE eraseLen #-}

-- | Inject a normal array into a fixed-length array, adding a compile-time
-- known length. This function is unsafe because it does not (and cannot) check
-- if the provided length is correct.
--
-- The function is often used with TypeApplications, for example:
-- >>> injectLenUnsafe @3 $ listArray (0, 2) [0, 1, (2 :: Int)]
-- length 3: array (0,2) [(0,0),(1,1),(2,2)]
injectLenUnsafe :: forall len e. IArray UArray e
                => UArray Int e -> UArrayFixed len e
injectLenUnsafe = UArrayFixed
{-# INLINE injectLenUnsafe #-}
