{-# LANGUAGE NoImplicitPrelude #-}

module MMZK.List.Fixed
  (
  -- * Fixed-length list
    ListFixed
  -- * Basic functions
  , head
  , tail
  , init
  , last
  , (++)
  , singleton
  , null
  , length
  , empty
  , cons
  , uncons
  , pattern Nil
  , pattern (:~)
  -- * List transformations
  , reverse
  , transpose
  , fmap
  , permutations
  -- * Reducing lists (folds)
  , concat
  , concatMap
  , foldl1'
  -- * Conversion
  , eraseLen
  -- * Utilities
  , Factorial
  ) where

import           Data.Foldable
import           Data.Functor
import           MMZK.List.Fixed.Internal
