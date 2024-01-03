module MMZK.Text where

import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Stack

-- | Similar to "Data.Text.breakOn", but drops the needle from the second
--   part of the result.
--
--  >>> splitPair "," "1,2,3"
-- Just ("1","2,3")
splitPair :: HasCallStack => Text -> Text -> Maybe (Text, Text)
splitPair needle haystack =
  case T.breakOn needle haystack of
    ~(a, b)
      | T.null b  -> Nothing
      | otherwise -> Just (a, T.drop (T.length needle) b) 
{-# INLINE splitPair #-}

-- | Similar to "Data.Text.breakOnEnd", but drops the needle from the first
--   part of the result.
--
--  >>> splitPairEnd "," "1,2,3"
-- Just ("1,2","3")
splitPairEnd :: HasCallStack => Text -> Text -> Maybe (Text, Text)
splitPairEnd needle haystack =
  case T.breakOnEnd needle haystack of
    ~(a, b)
      | T.null b  -> Nothing
      | otherwise -> Just (T.dropEnd (T.length needle) a, b)
{-# INLINE splitPairEnd #-}

-- | Pattern for matching a 'Text' that is empty.
pattern Null :: Text
pattern Null <- (T.null -> True)
  where
    Null = T.empty
{-# COMPLETE Null #-}

-- | Pattern match a 'Text' into its first character and the 'Data.Text.tail'.
pattern (:<|) :: Char -> Text -> Text
pattern x :<| xs <- (T.uncons -> Just (x, xs))
  where
    x :<| xs = T.cons x xs

-- | Pattern match a 'Text' into its 'Data.Text.init' and last character.
pattern (:|>) :: Text -> Char -> Text
pattern xs :|> x <- (T.unsnoc -> Just (xs, x))
  where
    xs :|> x = T.snoc xs x

{-# COMPLETE Null, (:<|) #-}
{-# COMPLETE Null, (:|>) #-}
