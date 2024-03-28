{-# LANGUAGE UndecidableInstances #-}

module MMZK.List.Fixed.Internal where

import           Data.Foldable
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeLits
import qualified Data.List as List
import           Prelude hiding (head, tail, init, last, null, reverse, (++))
import qualified Prelude

type family Factorial (n :: Nat) where
  Factorial 0 = 1
  Factorial n = n * Factorial (n - 1)

newtype ListFixed (len :: Nat) e = ListFixed [e]
  deriving newtype (Eq, Ord, Functor)

instance (Show e, KnownNat len) => Show (ListFixed len e) where
  showsPrec :: Int -> ListFixed len e -> ShowS
  showsPrec d (ListFixed xs) = ("length " <>)
                             . showsPrec d (natVal (Proxy @len))
                             . (": " <>) . showsPrec d xs
  {-# INLINE showsPrec #-}

instance KnownNat len => Foldable (ListFixed len) where
  foldMap :: Monoid m => (a -> m) -> ListFixed len a -> m
  foldMap f (ListFixed xs) = foldMap f xs
  {-# INLINE foldMap #-}

  length :: ListFixed len a -> Int
  length _ = fromIntegral (natVal (Proxy @len))
  {-# INLINE length #-}

  foldr :: (a -> b -> b) -> b -> ListFixed len a -> b
  foldr f z (ListFixed xs) = foldr f z xs
  {-# INLINE foldr #-}

instance (KnownNat len) => Traversable (ListFixed len) where
  traverse :: Applicative f
           => (a -> f b) -> ListFixed len a -> f (ListFixed len b)
  traverse f (ListFixed xs) = ListFixed <$> traverse f xs
  {-# INLINE traverse #-}

-- | Get the first element of a non-empty 'ListFixed'.
head :: len > 0 => ListFixed len e -> e
head (ListFixed xs) = Prelude.head xs
{-# INLINE head #-}

-- | Get all elements of a non-empty 'ListFixed' except the first one.
tail :: len > 0 => ListFixed len e -> ListFixed (len - 1) e
tail (ListFixed xs) = ListFixed (Prelude.tail xs)
{-# INLINE tail #-}

-- | Get all elements of a non-empty 'ListFixed' except the last one.
init :: len > 0 => ListFixed len e -> ListFixed (len - 1) e
init (ListFixed xs) = ListFixed (Prelude.init xs)
{-# INLINE init #-}

-- | Get the last element of a non-empty 'ListFixed'.
last :: len > 0 => ListFixed len e -> e
last (ListFixed xs) = Prelude.last xs
{-# INLINE last #-}

-- | Concatenate two 'ListFixed's.
(++) :: ListFixed n e -> ListFixed m e -> ListFixed (n + m) e
ListFixed xs ++ ListFixed ys = ListFixed (xs Prelude.++ ys)
infixr 5 ++
{-# INLINE (++) #-}

-- | The empty 'ListFixed'.
empty :: ListFixed 0 e
empty = ListFixed []
{-# INLINE empty #-}

-- | Construct a 'ListFixed' with a single element.
singleton :: e -> ListFixed 1 e
singleton x = ListFixed [x]
{-# INLINE singleton #-}

-- | Prepend an element to a 'ListFixed'.
cons :: e -> ListFixed n e -> ListFixed (n + 1) e
cons x (ListFixed xs) = ListFixed (x : xs)
{-# INLINE cons #-}

-- | Deconstruct a 'ListFixed' into its 'head' and 'tail'.
uncons :: ListFixed (n + 1) e -> (e, ListFixed n e)
uncons (ListFixed (x:xs)) = (x, ListFixed xs)
uncons _                  = error "impossible: uncons on empty list"
{-# INLINE uncons #-}

-- | Reverse a 'ListFixed'.
reverse :: ListFixed len e -> ListFixed len e
reverse (ListFixed xs) = ListFixed (Prelude.reverse xs)
{-# INLINE reverse #-}

-- | Transpose a 'ListFixed' of 'ListFixed's.
transpose :: ListFixed n (ListFixed m e) -> ListFixed m (ListFixed n e)
transpose (ListFixed xss) =
  ListFixed (ListFixed <$> List.transpose (eraseLen <$> xss))
{-# INLINE transpose #-}

-- | All permutations of a 'ListFixed'.
permutations :: ListFixed n e -> ListFixed (Factorial n) (ListFixed n e)
permutations (ListFixed xs) = ListFixed (ListFixed <$> List.permutations xs)
{-# INLINE permutations #-}

-- | Strict left-to-right fold of a non-empty 'ListFixed'.
foldl1' :: (e -> e -> e) -> ListFixed (n + 1) e -> e
foldl1' f (ListFixed (x:xs)) = List.foldl' f x xs
foldl1' _ _                  = error "impossible: foldl1' on empty list"
{-# INLINE foldl1' #-}

-- | Convert a 'ListFixed'' to a list, erasing the length information.
eraseLen :: ListFixed len e -> [e]
eraseLen (ListFixed xs) = xs
{-# INLINE eraseLen #-}

-- | Inject length information into a list, adding a compile-time known length.
-- The function is unsafe since it does not (and cannot) check if the provided
-- length is correct.
-- The function is often used with TypeApplications, for example:
-- >>> injectLenUnsafe @3 [0, 1, 2]
-- length 3: [0,1,2]
injectLenUnsafe :: forall len e. [e] -> ListFixed len e
injectLenUnsafe = ListFixed
{-# INLINE injectLenUnsafe #-}

-- | Pattern for matching an empty 'ListFixed'.
pattern Nil :: ListFixed 0 e
pattern Nil = ListFixed []

-- | Pattern for decomposing a non-empty 'ListFixed'.
infixr 5 :~
pattern (:~) :: e -> ListFixed n e -> ListFixed (n + 1) e
pattern x:~xs <- (uncons -> (x, xs))
  where
    x:~xs = cons x xs

{-# COMPLETE Nil, (:~) #-}
