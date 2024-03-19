module MMZK.List.Fixed.Internal where

import           Data.Foldable
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeLits
import           Prelude hiding (head, tail, init, last, null, reverse, (++))
import qualified Prelude

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

-- | Check if a 'ListFixed' is empty.
null :: ListFixed len e -> Bool
null (ListFixed xs) = Prelude.null xs
{-# INLINE null #-}

-- | Check if a 'ListFixed' is empty.
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
uncons :: ListFixed (n + 1) e -> Maybe (e, ListFixed n e)
uncons (ListFixed (x:xs)) = Just (x, ListFixed xs)
uncons _ = Nothing
{-# INLINE uncons #-}

-- | Append an element to a 'ListFixed'.
snoc :: ListFixed n e -> e -> ListFixed (n + 1) e
snoc (ListFixed xs) x = ListFixed (xs <> [x])
{-# INLINE snoc #-}

-- | Deconstruct a 'ListFixed' into its 'init' and 'last'.
unsnoc :: ListFixed (n + 1) e -> Maybe (ListFixed n e, e)
unsnoc (ListFixed xs) = case Prelude.reverse xs of
  []   -> Nothing
  y:ys -> Just (ListFixed (Prelude.reverse ys), y)
{-# INLINE unsnoc #-}

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

pattern Nil :: ListFixed 0 e
pattern Nil = ListFixed []

infixr 5 :~
pattern (:~) :: e -> ListFixed n e -> ListFixed (n + 1) e
pattern x:~xs <- (uncons -> Just (x, xs))
  where
    x:~xs = cons x xs
{-# COMPLETE Nil, (:~) #-}
