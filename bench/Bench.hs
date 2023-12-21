import           Criterion.Main
import           MMZK.Read.Internal
import           Text.Printf

parseWordTests :: [String]
parseWordTests = (show @Integer <$> ws)
              ++ (printf "0x%x" <$> ws)
              ++ (printf "0X%X" <$> ws)
  where
    w8   = [0, 2..255]
    w16  = (256 *) <$> w8
    w32  = (256 *) <$> w16
    w64  = (65536 *) <$> w32
    w128 = (4294967296 *) <$> w64
    ws   = [65535, 4294967295, 18446744073709551615] ++ w8 ++ w16 ++ w32 ++ w64 ++ w128

parseIntTests :: [String]
parseIntTests = (show @Integer <$> is)
             ++ (showHexSmall <$> is)
             ++ (showHexBig <$> is)
  where
    showHexSmall x
      | x >= 0    = printf "0x%x" x
      | otherwise = printf "-0x%x" (-x)
    showHexBig x
      | x >= 0    = printf "0X%X" x
      | otherwise = printf "-0X%X" (-x)
    i8   = [-128, -126..127]
    i16  = (256 *) <$> i8
    i32  = (256 *) <$> i16
    i64  = (65536 *) <$> i32
    i128 = (4294967296 *) <$> i64
    is   = [32767, 2147483647, 9223372036854775807] ++ i8 ++ i16 ++ i32 ++ i64 ++ i128



main :: IO ()
main = defaultMain
  [ env (pure parseWordTests) \testCases -> bgroup "Word Tests"
        [ 
          bgroup "Word8 parsing"
            [ bench "vanilla" $ nf (map readWord8Vanilla) testCases
            , bench "wheel" $ nf (map readWord8Wheel) testCases
            , bench "wheel'" $ nf (map readWord8Wheel') testCases
            ]
        , bgroup "Word16 parsing"
            [ bench "vanilla" $ nf (map readWord16Vanilla) testCases
            , bench "wheel" $ nf (map readWord16Wheel) testCases
            , bench "wheel'" $ nf (map readWord16Wheel') testCases
            ]
        , bgroup "Word32 parsing"
            [ bench "vanilla" $ nf (map readWord32Vanilla) testCases
            , bench "wheel" $ nf (map readWord32Wheel) testCases
            , bench "wheel'" $ nf (map readWord32Wheel') testCases
            ]
        , bgroup "Word64 parsing"
            [ bench "vanilla" $ nf (map readWord64Vanilla) testCases
            , bench "wheel" $ nf (map readWord64Wheel) testCases
            , bench "wheel'" $ nf (map readWord64Wheel') testCases
            ]
        , bgroup "Word parsing"
            [ bench "vanilla" $ nf (map readWordVanilla) testCases
            , bench "wheel" $ nf (map readWordWheel) testCases
            , bench "wheel'" $ nf (map readWordWheel') testCases
            ]
        ]
  , env (pure parseIntTests) \testCases -> bgroup "Int Tests"
        [ bgroup "Int8 parsing"
            [ bench "vanilla" $ nf (map readInt8Vanilla) testCases
            , bench "wheel" $ nf (map readInt8Wheel) testCases
            , bench "wheel'" $ nf (map readInt8Wheel') testCases
            ]
        , bgroup "Int16 parsing"
            [ bench "vanilla" $ nf (map readInt16Vanilla) testCases
            , bench "wheel" $ nf (map readInt16Wheel) testCases
            , bench "wheel'" $ nf (map readInt16Wheel') testCases
            ]
        , bgroup "Int32 parsing"
            [ bench "vanilla" $ nf (map readInt32Vanilla) testCases
            , bench "wheel" $ nf (map readInt32Wheel) testCases
            , bench "wheel'" $ nf (map readInt32Wheel') testCases
            ]
        , bgroup "Int64 parsing"
            [ bench "vanilla" $ nf (map readInt64Vanilla) testCases
            , bench "wheel" $ nf (map readInt64Wheel) testCases
            , bench "wheel'" $ nf (map readInt64Wheel') testCases
            ]
        , bgroup "Int parsing"
            [ bench "vanilla" $ nf (map readIntVanilla) testCases
            , bench "wheel" $ nf (map readIntWheel) testCases
            , bench "wheel'" $ nf (map readIntWheel') testCases
            ]
        ]
  ]
