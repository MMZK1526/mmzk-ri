-- | Fixed-length arrays with compile-time known length.
module MMZK.Array.Fixed
  (
  -- * Fixed-length array
    ArrayFixed
  -- * Element access
  , readArr
  -- * Conversion
  , eraseLen
  ) where

import           MMZK.Array.Fixed.Internal
