-- | Fixed-length unboxed arrays with compile-time known length.
module MMZK.Array.Unboxed.Fixed
  (
  -- * Fixed-length array
    UArrayFixed
  -- * Element access
  , readArr
  -- * Conversion
  , eraseLen
  ) where

import           MMZK.Array.Unboxed.Fixed.Internal
