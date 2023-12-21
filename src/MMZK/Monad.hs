module MMZK.Monad
  ( module MMZK.Monad
  , module Control.Monad
  ) where

import           Control.Monad.Fix
import           Control.Monad

-- | Monadic version of 'when'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb m = mb >>= (`when` m)
{-# INLINE whenM #-}

-- | Monadic version of 'unless'.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb m = mb >>= (`unless` m)
{-# INLINE unlessM #-}

-- | A do-while loop. The loop body evaluates to a @Bool@ value. If it is
-- @True@, the loop continues; otherwise, the loop terminates.
doWhileM :: Monad m => m Bool -> m ()
doWhileM = fix . whenM
{-# INLINE doWhileM #-}

-- | A while loop. The condition evaluates to a @Bool@ value. If it is @True@,
-- the loop body is executed and the loop continues; otherwise, the loop
-- terminates.
whileM :: Monad m => m Bool -> m a -> m ()
whileM p m = m >> doWhileM (p >>= (<$ m))
{-# INLINE whileM #-}
