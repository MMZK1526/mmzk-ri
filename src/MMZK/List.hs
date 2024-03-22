{-# LANGUAGE ExistentialQuantification #-}

-- | This module provides some helper functions that are useful for list
-- operations.
module MMZK.List where

-- | The 'LenCompare' type is used to compare lists by their length. It is a
-- more efficient implementation than by comparing 'length' values directly
-- since it does not always need to traverse the whole list.
data LenCompare = forall a. LenCompare [a]

instance Eq LenCompare where
  (==) :: LenCompare -> LenCompare -> Bool
  LenCompare (_:xs) == LenCompare (_:ys) = LenCompare xs == LenCompare ys
  LenCompare [] == LenCompare []         = True
  _ == _                                 = False
  {-# INLINE (==) #-}

instance Ord LenCompare where
  compare :: LenCompare -> LenCompare -> Ordering
  compare (LenCompare (_:xs)) (LenCompare (_:ys))
    = compare (LenCompare xs) (LenCompare ys)
  compare (LenCompare []) (LenCompare []) = EQ
  compare (LenCompare []) _               = LT
  compare _ (LenCompare [])               = GT
  {-# INLINE compare #-}

-- | Compare two lists by their length.
lenCompare :: [a] -> [b] -> Ordering
lenCompare as bs = compare (LenCompare as) (LenCompare bs)
{-# INLINE lenCompare #-}

-- | Compare two lists of the same type by their length.
lenCompare' :: [a] -> [a] -> Ordering
lenCompare' = lenCompare
{-# INLINE lenCompare' #-}

-- | Compare a list's length to a number.
lenCompareNum :: Integral n => [a] -> n -> Ordering
lenCompareNum [] n = compare 0 n
lenCompareNum bs n
  | notNull ys     = GT
  | length xs < n' = LT
  | otherwise      = EQ
  where
    n'       = fromIntegral n
    (xs, ys) = splitAt n' bs
{-# INLINE lenCompareNum #-}

-- | @notNull xs === not (null xs)@.
notNull :: [a] -> Bool
notNull = not . null
{-# INLINE notNull #-}
