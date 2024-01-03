{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module MMZK.List.Fixed.Internal where

import           Data.Foldable

data Peano = Z | S Peano

type family Plus (n :: Peano) (m :: Peano) :: Peano where
  Plus Z     m = m
  Plus (S n) m = S (Plus n m)

type family Minus (n :: Peano) (m :: Peano) :: Peano where
  Minus n     Z     = n
  Minus (S n) (S m) = Minus n m

data ListFixed (len :: Peano) e where
  Nil  :: ListFixed Z e
  (:~) :: e -> ListFixed n e -> ListFixed (S n) e

instance Eq e => Eq (ListFixed len e) where
  (==) :: ListFixed len e -> ListFixed len e -> Bool
  Nil == Nil             = True
  (x :~ xs) == (y :~ ys) = x == y && xs == ys
  {-# INLINE (==) #-}

instance Ord e => Ord (ListFixed len e) where
  compare :: ListFixed len e -> ListFixed len e -> Ordering
  compare Nil Nil             = EQ
  compare (x :~ xs) (y :~ ys) = compare x y <> compare xs ys
  {-# INLINE compare #-}

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

head :: ListFixed (S n) e -> e
head (x :~ _) = x
{-# INLINE head #-}

tail :: ListFixed (S n) e -> ListFixed n e
tail (_ :~ xs) = xs
{-# INLINE tail #-}

init :: ListFixed (S n) e -> ListFixed n e
init (_ :~ Nil)        = Nil
init (x :~ (x' :~ xs)) = x :~ MMZK.List.Fixed.Internal.init (x' :~ xs)
{-# INLINE init #-}

last :: ListFixed (S n) e -> e
last (x :~ Nil)        = x
last (_ :~ (x' :~ xs)) = MMZK.List.Fixed.Internal.last (x' :~ xs)
{-# INLINE last #-}

(++) :: ListFixed n e -> ListFixed m e -> ListFixed (Plus n m) e
Nil ++ ys       = ys
(x :~ xs) ++ ys = x :~ (xs MMZK.List.Fixed.Internal.++ ys)
{-# INLINE (++) #-}

null :: ListFixed len e -> Bool
null Nil = True
null _   = False
{-# INLINE null #-}

singleton :: e -> ListFixed (S Z) e
singleton x = x :~ Nil
{-# INLINE singleton #-}
