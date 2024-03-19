{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Internal module for 'MMZK.Array.Fixed'. The functions in this module are
-- not exposed by the public API except for the type class methods.
module MMZK.Array.Fixed.Internal where

import           Data.Array.Base
import           Data.Array.IArray
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeLits

-- | A fixed-length array. The length is known at compile time.
--
-- The 'readArr' function allows us to read from a compile-time known index
-- without runtime bounds checking.
newtype ArrayFixed (len :: Nat) ix e = ArrayFixed (Array ix e)
  deriving newtype (Eq, Ord, Functor)

instance KnownNat len => Foldable (ArrayFixed len ix) where
  foldMap :: Monoid m => (a -> m) -> ArrayFixed len ix a -> m
  foldMap f (ArrayFixed arr) = foldMap f arr
  {-# INLINE foldMap #-}

  length :: ArrayFixed len ix a -> Int
  length _ = fromIntegral (natVal (Proxy @len))
  {-# INLINE length #-}

instance (Show e, Show ix, Ix ix, KnownNat len)
  => Show (ArrayFixed len ix e) where
    showsPrec :: Int -> ArrayFixed len ix e -> ShowS
    showsPrec d (ArrayFixed arr) = ("length " ++)
                                 . showsPrec d (natVal (Proxy @len))
                                 . (' ' :) . showsPrec d arr
    {-# INLINE showsPrec #-}

instance KnownNat len => IArray (ArrayFixed len) e where
  bounds :: Ix ix => ArrayFixed len ix e -> (ix, ix)
  bounds (ArrayFixed arr) = bounds arr
  {-# INLINE bounds #-}

  numElements :: Ix ix => ArrayFixed len ix e -> Int
  numElements _ = fromIntegral (natVal (Proxy @len))
  {-# INLINE numElements #-}

  unsafeArray :: Ix ix => (ix, ix) -> [(Int, e)] -> ArrayFixed len ix e
  unsafeArray = (ArrayFixed .) . unsafeArray
  {-# INLINE unsafeArray #-}

  unsafeAt :: Ix ix => ArrayFixed len ix e -> Int -> e
  unsafeAt (ArrayFixed arr) = unsafeAt arr
  {-# INLINE unsafeAt #-}

instance (Ix ix, KnownNat len) => Traversable (ArrayFixed len ix) where
  traverse :: Applicative f
           => (a -> f b) -> ArrayFixed len ix a -> f (ArrayFixed len ix b)
  traverse f (ArrayFixed arr) = ArrayFixed <$> traverse f arr
  {-# INLINE traverse #-}

-- | Read an element from a compile-time known index without runtime bounds
-- checking.
readArr :: forall loc len ix e
        . (Ix ix, KnownNat loc, KnownNat len, loc >= 0, loc < len)
       => ArrayFixed len ix e -> e
readArr = readArr' @loc
{-# INLINE readArr #-}

readArr' :: forall loc len e ix
         . (KnownNat loc, KnownNat len, Ix ix, loc >= 0, loc < len)
        => ArrayFixed len ix e -> e
readArr' arr = unsafeAt arr (fromIntegral (natVal (Proxy @loc)))
{-# INLINE readArr' #-}

-- | Convert a fixed-length array to a normal array, effectively erasing the
-- compile-time known length.
eraseLen :: Ix ix => ArrayFixed len ix e -> Array ix e
eraseLen (ArrayFixed arr) = arr
{-# INLINE eraseLen #-}

-- | Inject a normal array into a fixed-length array, adding a compile-time
-- known length. This function is unsafe because it does not (and cannot) check
-- if the provided length is correct.
--
-- The function is often used with TypeApplications, for example:
-- >>> injectLenUnsafe @3 $ listArray (0, 2) [0, 1, 2]
-- length 3 array (0,2) [(0,0),(1,1),(2,2)]
injectLenUnsafe :: forall len ix e. Ix ix => Array ix e -> ArrayFixed len ix e
injectLenUnsafe = ArrayFixed
{-# INLINE injectLenUnsafe #-}
