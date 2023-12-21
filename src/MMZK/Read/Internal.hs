-- | Internal implementations for reading various integral types. It has several
-- implementations for each type, and the fastest one (based on benchmark) is
-- selected (hardcoded at the type class instance).
module MMZK.Read.Internal
  ( module MMZK.Read.Internal
  , module Data.Int
  , module Data.Word
  ) where

import           Data.Int
import           Data.Word
import           MMZK.Function
import qualified Text.Read

pattern Hex :: String -> String
pattern Hex str <- (extractHex -> Just str)

extractHex :: String -> Maybe String
extractHex str = case str of
  '0' : 'x' : rest -> Just rest
  '0' : 'X' : rest -> Just rest
  _                -> Nothing
{-# INLINE extractHex #-}

digit2Num :: Num a => Char -> Maybe a
digit2Num c = guard (isDigit c) $> fromIntegral (ord c - ord '0')
{-# INLINE digit2Num #-}

digit2Hex :: Num a => Char -> Maybe a
digit2Hex c = guard (isHexDigit c) $> fromIntegral hexC
  where
    hexC = if isDigit c then ord c - ord '0' else ord (toLower c) - ord 'a' + 10
{-# INLINE digit2Hex #-}

two7 :: Integer
two7 = 128

two7Div10 :: Int8
two7Div10 = 12

two7Div16 :: Int8
two7Div16 = 8

two8 :: Integer
two8 = 256

two8Div10 :: Word8
two8Div10 = 25

two8Div16 :: Word8
two8Div16 = 16

two15 :: Integer
two15 = 32768

two15Div10 :: Int16
two15Div10 = 3276

two15Div16 :: Int16
two15Div16 = 2048

two16 :: Integer
two16 = 65536

two16Div10 :: Word16
two16Div10 = 6553

two16Div16 :: Word16
two16Div16 = 4096

two31 :: Integer
two31 = 2147483648

two31Div10 :: Int32
two31Div10 = 214748364

two31Div16 :: Int32
two31Div16 = 134217728

two32 :: Integer
two32 = 4294967296

two32Div10 :: Word32
two32Div10 = 429496729

two32Div16 :: Word32
two32Div16 = 268435456

two63 :: Integer
two63 = 9223372036854775808

two63Div10 :: Int64
two63Div10 = 922337203685477580

two63Div16 :: Int64
two63Div16 = 576460752303423488

two64 :: Integer
two64 = 18446744073709551616

two64Div10 :: Word64
two64Div10 = 1844674407370955161

two64Div16 :: Word64
two64Div16 = 1152921504606846976

wordMax, intMax :: Integer
wordMax = fromIntegral (maxBound :: Word)
intMax  = fromIntegral (maxBound :: Int)
{-# INLINE wordMax #-}
{-# INLINE intMax #-}

wordMaxDiv10 :: Word
wordMaxDiv10 = case maxBound `quotRem` 10 of
  (q, 9) -> q + 1
  (q, _) -> q
{-# INLINE wordMaxDiv10 #-}

wordMaxDiv16 :: Word
wordMaxDiv16 = case maxBound `quotRem` 16 of
  (q, 15) -> q + 1
  (q, _)  -> q
{-# INLINE wordMaxDiv16 #-}

intMaxDiv10 :: Int
intMaxDiv10 = case maxBound `quotRem` 10 of
  (q, 9) -> q + 1
  (q, _) -> q
{-# INLINE intMaxDiv10 #-}

intMaxDiv16 :: Int
intMaxDiv16 = case maxBound `quotRem` 16 of
  (q, 15) -> q + 1
  (q, _)  -> q
{-# INLINE intMaxDiv16 #-}

readWord8Vanilla :: String -> Either String Word8
readWord8Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= 0 && i < two8
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Word8 out of range"

readWord8Wheel :: String -> Either String Word8
readWord8Wheel str
  | Hex hex <- str = base16PRead two8Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = fromIntegral <$> foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two8) $ Left "MMZK.Read.readEither: Word8 out of range"
      pure result

readWord8Wheel' :: String -> Either String Word8
readWord8Wheel' str
  | Hex hex <- str = base16PRead two8Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two8Div10 || (num == two8Div10 && digit <= 5))
        $ Left "MMZK.Read.readEither: Word8 out of range"
      pure $ num * 10 + digit

readWord16Vanilla :: String -> Either String Word16
readWord16Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= 0 && i < two16
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Word16 out of range"

readWord16Wheel :: String -> Either String Word16
readWord16Wheel str
  | Hex hex <- str = base16PRead two16Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = fromIntegral <$> foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two16) $ Left "MMZK.Read.readEither: Word16 out of range"
      pure result

readWord16Wheel' :: String -> Either String Word16
readWord16Wheel' str
  | Hex hex <- str = base16PRead two16Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two16Div10 || (num == two16Div10 && digit <= 5))
        $ Left "MMZK.Read.readEither: Word16 out of range"
      pure $ num * 10 + digit

readWord32Vanilla :: String -> Either String Word32
readWord32Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= 0 && i < two32
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Word32 out of range"

readWord32Wheel :: String -> Either String Word32
readWord32Wheel str
  | Hex hex <- str = base16PRead two32Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = fromIntegral <$> foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two32) $ Left "MMZK.Read.readEither: Word32 out of range"
      pure result

readWord32Wheel' :: String -> Either String Word32
readWord32Wheel' str
  | Hex hex <- str = base16PRead two32Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two32Div10 || (num == two32Div10 && digit <= 5))
        $ Left "MMZK.Read.readEither: Word32 out of range"
      pure $ num * 10 + digit

readWord64Vanilla :: String -> Either String Word64
readWord64Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= 0 && i < two64
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Word64 out of range"

readWord64Wheel :: String -> Either String Word64
readWord64Wheel str
  | Hex hex <- str = base16PRead two64Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = fromIntegral <$> foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two64) $ Left "MMZK.Read.readEither: Word64 out of range"
      pure result

readWord64Wheel' :: String -> Either String Word64
readWord64Wheel' str
  | Hex hex <- str = base16PRead two64Div16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two64Div10 || (num == two64Div10 && digit <= 5))
        $ Left "MMZK.Read.readEither: Word64 out of range"
      pure $ num * 10 + digit

readWordVanilla :: String -> Either String Word
readWordVanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= 0 && i <= wordMax
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Word out of range"

readWordWheel :: String -> Either String Word
readWordWheel str
  | Hex hex <- str = base16PRead wordMaxDiv16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = fromIntegral <$> foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result <= wordMax) $ Left "MMZK.Read.readEither: Word out of range"
      pure result

readWordWheel' :: String -> Either String Word
readWordWheel' str
  | Hex hex <- str = base16PRead wordMaxDiv16 hex
  | null str       = Left "MMZK.Read.readEither: parse error"
  | otherwise      = foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless ( num < wordMaxDiv10
            || (num == wordMaxDiv10 && digit <= maxBound `mod` 10) )
        $ Left "MMZK.Read.readEither: Word out of range"
      pure $ num * 10 + digit

readInt8Vanilla :: String -> Either String Int8
readInt8Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= -two7 && i < two7
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Int8 out of range"

readInt8Wheel :: String -> Either String Int8
readInt8Wheel str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two7Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate . fromIntegral <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two7Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = fromIntegral <$> foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two7) $ Left "MMZK.Read.readEither: Int8 out of range"
      pure result
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result <= two7) $ Left "MMZK.Read.readEither: Int8 out of range"
      pure result

readInt8Wheel' :: String -> Either String Int8
readInt8Wheel' str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two7Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two7Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two7Div10 || (num == two7Div10 && digit <= 7))
        $ Left "MMZK.Read.readEither: Int8 out of range"
      pure $ num * 10 + digit
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num - 1 < two7Div10 - 1 || (num == two7Div10 && digit <= 8))
        $ Left "MMZK.Read.readEither: Int8 out of range"
      pure $ num * 10 + digit

readInt16Vanilla :: String -> Either String Int16
readInt16Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= -two15 && i < two15
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Int16 out of range"

readInt16Wheel :: String -> Either String Int16
readInt16Wheel str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two15Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate . fromIntegral <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two15Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = fromIntegral <$> foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two15) $ Left "MMZK.Read.readEither: Int16 out of range"
      pure result
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result <= two15) $ Left "MMZK.Read.readEither: Int16 out of range"
      pure result

readInt16Wheel' :: String -> Either String Int16
readInt16Wheel' str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two15Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two15Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two15Div10 || (num == two15Div10 && digit <= 7))
        $ Left "MMZK.Read.readEither: Int16 out of range"
      pure $ num * 10 + digit
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num - 1 < two15Div10 - 1 || (num == two15Div10 && digit <= 8))
        $ Left "MMZK.Read.readEither: Int16 out of range"
      pure $ num * 10 + digit

readInt32Vanilla :: String -> Either String Int32
readInt32Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= -two31 && i < two31
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Int32 out of range"

readInt32Wheel :: String -> Either String Int32
readInt32Wheel str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two31Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate . fromIntegral <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two31Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = fromIntegral <$> foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two31) $ Left "MMZK.Read.readEither: Int32 out of range"
      pure result
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result <= two31) $ Left "MMZK.Read.readEither: Int32 out of range"
      pure result

readInt32Wheel' :: String -> Either String Int32
readInt32Wheel' str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two31Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two31Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two31Div10 || (num == two31Div10 && digit <= 7))
        $ Left "MMZK.Read.readEither: Int32 out of range"
      pure $ num * 10 + digit
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num - 1 < two31Div10 - 1 || (num == two31Div10 && digit <= 8))
        $ Left "MMZK.Read.readEither: Int32 out of range"
      pure $ num * 10 + digit

readInt64Vanilla :: String -> Either String Int64
readInt64Vanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= -two63 && i < two63
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Integer out of range"

readInt64Wheel :: String -> Either String Int64
readInt64Wheel str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two63Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate . fromIntegral <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two63Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = fromIntegral <$> foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result < two63) $ Left "MMZK.Read.readEither: Integer out of range"
      pure result
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result <= two63) $ Left "MMZK.Read.readEither: Integer out of range"
      pure result

readInt64Wheel' :: String -> Either String Int64
readInt64Wheel' str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead two63Div16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead two63Div16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < two63Div10 || (num == two63Div10 && digit <= 7))
        $ Left "MMZK.Read.readEither: Integer out of range"
      pure $ num * 10 + digit
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num - 1 < two63Div10 - 1 || (num == two63Div10 && digit <= 8))
        $ Left "MMZK.Read.readEither: Integer out of range"
      pure $ num * 10 + digit

readIntVanilla :: String -> Either String Int
readIntVanilla str = case Text.Read.readEither @Integer str of
  Left  err -> Left err
  Right i   -> if i >= -intMax - 1 && i <= intMax
    then Right (fromIntegral i)
    else Left "MMZK.Read.readEither: Int out of range"

readIntWheel :: String -> Either String Int
readIntWheel str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead intMaxDiv16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate . fromIntegral <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead intMaxDiv16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = fromIntegral <$> foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result <= intMax) $ Left "MMZK.Read.readEither: Int out of range"
      pure result
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      let result = num * 10 + digit
      unless (result <= intMax + 1) $ Left "MMZK.Read.readEither: Int out of range"
      pure result

readIntWheel' :: String -> Either String Int
readIntWheel' str
  | '-' : rmn <- str = case rmn of
    Hex hex -> base16NRead intMaxDiv16 hex
    ""      -> Left "MMZK.Read.readEither: parse error"
    _       -> negate <$> foldM worker' 0 rmn
  | Hex hex <- str   = base16PRead intMaxDiv16 hex
  | null str         = Left "MMZK.Read.readEither: parse error"
  | otherwise        = foldM worker 0 str
  where
    worker num ch  = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless ( num < intMaxDiv10
           || (num == intMaxDiv10 && digit <= maxBound `mod` 10) )
        $ Left "MMZK.Read.readEither: Int out of range"
      pure $ num * 10 + digit
    worker' num ch = do
      digit <- case digit2Num ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless ( num - 1 < intMaxDiv10 - 1
           || (num == intMaxDiv10 && digit <= 10 - minBound `mod` 10) )
        $ Left "MMZK.Read.readEither: Int out of range"
      pure $ num * 10 + digit

base16PRead :: (Num a, Ord a) => a -> String -> Either String a
base16PRead _ ""   = Left "MMZK.Read.readEither: parse error"
base16PRead ub str = foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Hex ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num < ub) $ Left "MMZK.Read.readEither: out of range"
      pure $ num * 16 + digit

base16NRead :: (Num a, Ord a) => a -> String -> Either String a
base16NRead _ ""   = Left "MMZK.Read.readEither: parse error"
base16NRead ub str = negate <$> foldM worker 0 str
  where
    worker num ch = do
      digit <- case digit2Hex ch of
        Just d  -> pure d
        Nothing -> Left "MMZK.Read.readEither: parse error"
      unless (num - 1 < ub - 1 || (num == ub && digit == 0))
        $ Left "MMZK.Read.readEither: out of range"
      pure $ num * 16 + digit
