-- | Simple functions for converting between 'Maybe' and 'Either'.
module MMZK.Maybe
  ( module MMZK.Maybe
  , module Data.Maybe
  ) where

import           Data.Maybe

-- | Convert 'Either' to 'Maybe', retaining the 'Right' value.
e2m :: Either a b -> Maybe b
e2m = either (const Nothing) Just
{-# INLINE e2m #-}

-- | Convert 'Either' to 'Maybe', retaining the 'Right' value.
er2m :: Either a b -> Maybe b
er2m = e2m
{-# INLINE er2m #-}

-- | Convert 'Either' to 'Maybe', retaining the 'Left' value.
el2m :: Either a b -> Maybe a
el2m = either Just (const Nothing)
{-# INLINE el2m #-}

-- | Convert 'Maybe' to 'Either', using the given value as the 'Left' value.
m2e :: a -> Maybe b -> Either a b
m2e a = maybe (Left a) Right
{-# INLINE m2e #-}

-- | Convert 'Maybe' to 'Either', using the given value as the 'Left' value.
m2er :: a -> Maybe b -> Either a b
m2er = m2e
{-# INLINE m2er #-}

-- | Convert 'Maybe' to 'Either', using the given value as the 'Right' value.
m2el :: a -> Maybe b -> Either b a
m2el a = maybe (Right a) Left
{-# INLINE m2el #-}
