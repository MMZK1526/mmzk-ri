{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module MMZK.List.Fixed.Internal where

import           Data.Foldable
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeLits

type family IsZero (n :: Nat) :: Bool where
  IsZero 0 = 'True
  IsZero _ = 'False

class Eq' a (b :: Bool) where
  eq' :: Proxy b -> a -> a -> Bool

class Ord' a (b :: Bool) where
  compare' :: Proxy b -> a -> a -> Ordering

infixr 5 :~
data ListFixed (len :: Nat) e where
  Nil  :: ListFixed 0 e
  (:~) :: n > 0 => e -> ListFixed (n - 1) e -> ListFixed n e

instance (Eq e, Eq' (ListFixed (len :: Nat) e) (IsZero len))
  => Eq (ListFixed len e) where
    (==) :: ListFixed len e -> ListFixed len e -> Bool
    (==) = eq' (Proxy :: Proxy (IsZero len))
    {-# INLINE (==) #-}

instance ( Ord e, Eq (ListFixed (len :: Nat) e)
         , Ord' (ListFixed (len :: Nat) e) (IsZero len)
         ) =>
  Ord (ListFixed len e) where
    compare :: ListFixed len e -> ListFixed len e -> Ordering
    compare = compare' (Proxy :: Proxy (IsZero len))
    {-# INLINE compare #-}

instance Eq' (ListFixed 0 e) 'True where
  eq' :: Proxy 'True -> ListFixed 0 e -> ListFixed 0 e -> Bool
  eq' _ _ _ = True
  {-# INLINE eq' #-}

instance (Eq e, Eq' (ListFixed (n - 1) e) (IsZero (n - 1)), n > 0)
  => Eq' (ListFixed n e) 'False where
    eq' :: Proxy 'False -> ListFixed n e -> ListFixed n e -> Bool
    eq' _ (x :~ xs) (y :~ ys) = x == y && eq' (Proxy @(IsZero (n - 1))) xs ys
    {-# INLINE eq' #-}

instance Ord' (ListFixed 0 e) 'True where
  compare' :: Proxy 'True -> ListFixed 0 e -> ListFixed 0 e -> Ordering
  compare' _ _ _ = EQ
  {-# INLINE compare' #-}

instance ( Ord e, Ord (ListFixed (n - 1) e)
         , Ord' (ListFixed (n - 1) e) (IsZero (n - 1)), n > 0
         ) => Ord' (ListFixed n e) 'False where
    compare' :: Proxy 'False -> ListFixed n e -> ListFixed n e -> Ordering
    compare' _ (x :~ xs) (y :~ ys)
      = compare x y <> compare' (Proxy @(IsZero (n - 1))) xs ys
    {-# INLINE compare' #-}

instance Show e => Show (ListFixed len e) where
  show :: ListFixed len e -> String
  show = show . toList
  {-# INLINE show #-}

instance Foldable (ListFixed len) where
  foldMap :: Monoid m => (a -> m) -> ListFixed len a -> m
  foldMap _ Nil       = mempty
  foldMap f (x :~ xs) = f x <> foldMap f xs
  {-# INLINE foldMap #-}

  length :: ListFixed len a -> Int
  length Nil       = 0
  length (_ :~ xs) = 1 + length xs
  {-# INLINE length #-}

instance Functor (ListFixed len) where
  fmap :: (a -> b) -> ListFixed len a -> ListFixed len b
  fmap _ Nil       = Nil
  fmap f (x :~ xs) = f x :~ fmap f xs
  {-# INLINE fmap #-}

instance Traversable (ListFixed len) where
  traverse :: Applicative f
           => (a -> f b) -> ListFixed len a -> f (ListFixed len b)
  traverse _ Nil       = pure Nil
  traverse f (x :~ xs) = (:~) <$> f x <*> traverse f xs
  {-# INLINE traverse #-}

head :: n > 0 => ListFixed n e -> e
head (x :~ _) = x
{-# INLINE head #-}

tail :: n > 0 => ListFixed n e -> ListFixed (n - 1) e
tail (_ :~ xs) = xs
{-# INLINE tail #-}

init :: n > 0 => ListFixed n e -> ListFixed (n - 1) e
init (_ :~ Nil)          = Nil
init (x :~ xs'@(_ :~ _)) = x :~ MMZK.List.Fixed.Internal.init xs'
{-# INLINE init #-}

last :: n > 0 => ListFixed n e -> e
last (x :~ Nil)        = x
last (_ :~ (x' :~ xs)) = MMZK.List.Fixed.Internal.last (x' :~ xs)
{-# INLINE last #-}

-- (++) :: ListFixed n e -> ListFixed m e -> ListFixed (Plus n m) e
-- Nil ++ ys       = ys
-- (x :~ xs) ++ ys = x :~ (xs MMZK.List.Fixed.Internal.++ ys)
-- {-# INLINE (++) #-}

null :: ListFixed len e -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

singleton :: e -> ListFixed 1 e
singleton x = x :~ Nil
{-# INLINE singleton #-}
