-- | Fixed-length arrays with compile-time known length.
module MMZK.Array.Fixed
  (
  -- * Fixed-length arrays
    ArrayFixed
  -- * Element access
  , readArr
  -- * Normal array/list conversion
  , eraseLen
  ) where

import           MMZK.Array.Fixed.Internal
