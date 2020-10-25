{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

import           Control.DeepSeq               (NFData)
import           Criterion                     (Benchmark, nf)
import           Data.Aeson.Combinators.Decode (Decoder)
import           Data.Aeson.Types              (FromJSON, ToJSON, withObject,
                                                (.:))
import           Data.ByteString.Lazy          (ByteString)
import           GHC.Generics                  (Generic)

import qualified Criterion.Main                as Criterion
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Combinators.Decode as Decode


bench :: NFData b => String -> (a -> b) -> (Int -> a) -> [Benchmark]
bench name f gen = fmap (\i -> let !generated = gen i
                               in Criterion.bench (name <> " " <> show i) $ (nf f) generated
                        ) [10,100..1000]
{-# INLINE bench #-}


main :: IO ()
main =
  Criterion.defaultMain
    [ Criterion.bgroup "Combinators decoder nested" $
        bench "nested" (Decode.decode deeplyNestedDecoder) deeplyNestedValue
    , Criterion.bgroup "Derived (generic) decoder nested" $
        bench "nested" (Aeson.decode @ DeeplyNested) deeplyNestedValue
    , Criterion.bgroup "Combinators decoder narrow" $
        bench "narrow" (Decode.decode narrowDecoder) narrowValue
    , Criterion.bgroup "Dreived (generic) decoder narrow" $
        bench "narrow" (Aeson.decode @ Narrow) narrowValue
    ]


-- | Wrapper for implementing alternative instance for Aeson Typeclasses
newtype Implement a = Implement a


data DeeplyNested = DeeplyNested
    { nested :: ![DeeplyNested]
    } deriving stock (Show, Generic)
      deriving anyclass (FromJSON, NFData, ToJSON)


instance FromJSON (Implement DeeplyNested) where
  parseJSON = withObject "DeeplyNested" $ \v ->
    Implement . DeeplyNested <$> v .: "nested"


deeplyNestedDecoder :: Decoder DeeplyNested
deeplyNestedDecoder = DeeplyNested
  <$> Decode.key "nested" (Decode.list deeplyNestedDecoder)


deeplyNestedValue :: Int -> ByteString
deeplyNestedValue depth = Aeson.encode $ go depth (DeeplyNested [])
  where
    go :: Int -> DeeplyNested -> DeeplyNested
    go n dn
      | n <= 0 = dn
      | otherwise = go (n - 1) $ DeeplyNested [dn]


data Narrow = Narrow
    { narrow :: ![Int]
    } deriving stock (Show, Generic)
      deriving anyclass (FromJSON, NFData, ToJSON)


instance FromJSON (Implement Narrow) where
  parseJSON = withObject "DeeplyNested" $ \v ->
    Implement . Narrow <$> v .: "narrow"


narrowDecoder :: Decoder Narrow
narrowDecoder = Narrow
  <$> Decode.key "narrow" (Decode.list Decode.int)


narrowValue :: Int -> ByteString
narrowValue width = Aeson.encode $ Narrow [1..width]
