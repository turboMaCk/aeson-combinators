
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Combinators.Decode as ACD
import Criterion.Main
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

-- Our benchmark harness.
main = do
  let !nestedVal10 = deeplyNestedValue 10
      !nestedVal100 = deeplyNestedValue 100
      !nestedVal1000 = deeplyNestedValue 1000
      !nestedVal10000 = deeplyNestedValue 10000
      !narrowVal10 = narrowValue 10
      !narrowVal100 = narrowValue 100
      !narrowVal1000 = narrowValue 1000
      !narrowVal10000 = narrowValue 10000
  defaultMain [
    bgroup "ACD decoder nested"
      [ bench "nested 10"    $ nf (ACD.decode deeplyNestedDecoder) nestedVal10
      , bench "nested 100"   $ nf (ACD.decode deeplyNestedDecoder) nestedVal100
      , bench "nested 1000"  $ nf (ACD.decode deeplyNestedDecoder) nestedVal1000
      , bench "nested 10000" $ nf (ACD.decode deeplyNestedDecoder) nestedVal10000
      ],
    bgroup "Aeson decoder"
      [ bench "nested 10"    $ nf (Aeson.decode :: ByteString -> Maybe DeeplyNested) nestedVal10
      , bench "nested 100"   $ nf (Aeson.decode :: ByteString -> Maybe DeeplyNested) nestedVal100
      , bench "nested 1000"  $ nf (Aeson.decode :: ByteString -> Maybe DeeplyNested) nestedVal1000
      , bench "nested 10000" $ nf (Aeson.decode :: ByteString -> Maybe DeeplyNested) nestedVal10000
      ],
    bgroup "ACD decoder narrow"
      [ bench "narrow 10"    $ nf (ACD.decode narrowDecoder) narrowVal10
      , bench "narrow 100"   $ nf (ACD.decode narrowDecoder) narrowVal100
      , bench "narrow 1000"  $ nf (ACD.decode narrowDecoder) narrowVal1000
      , bench "narrow 10000" $ nf (ACD.decode narrowDecoder) narrowVal10000
      ],
    bgroup "Aeson decoder"
      [ bench "narrow 10"    $ nf (Aeson.decode :: ByteString -> Maybe Narrow) narrowVal10
      , bench "narrow 100"   $ nf (Aeson.decode :: ByteString -> Maybe Narrow) narrowVal100
      , bench "narrow 1000"  $ nf (Aeson.decode :: ByteString -> Maybe Narrow) narrowVal1000
      , bench "narrow 10000" $ nf (Aeson.decode :: ByteString -> Maybe Narrow) narrowVal10000
      ]
    ]

data DeeplyNested = DeeplyNested
    { nested :: [DeeplyNested]
    }
    deriving (Show, Generic)

instance NFData DeeplyNested

instance ToJSON DeeplyNested where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON DeeplyNested

deeplyNestedDecoder :: ACD.Decoder DeeplyNested
deeplyNestedDecoder = DeeplyNested <$> (ACD.key "nested" $ ACD.list deeplyNestedDecoder)

deeplyNestedValue :: Int -> ByteString
deeplyNestedValue depth = Aeson.encode $ go depth (DeeplyNested [])
  where
    go :: Int -> DeeplyNested -> DeeplyNested
    go n dn
      | n <= 0 = dn
      | otherwise = go (n - 1) (DeeplyNested [dn])

data Narrow = Narrow
    { narrow :: [Int]
    }
    deriving (Show, Generic)

instance NFData Narrow

instance ToJSON Narrow where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON Narrow

narrowDecoder :: ACD.Decoder Narrow
narrowDecoder = Narrow <$> (ACD.key "narrow" $ ACD.list ACD.int)

narrowValue :: Int -> ByteString
narrowValue width = Aeson.encode $ Narrow [1..width]