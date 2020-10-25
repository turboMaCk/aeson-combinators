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
bench name f gen = fmap (\i -> let !n         = 10 ^ i
                                   !generated = gen n
                               in Criterion.bench (name <> " " <> show n) $ (nf f) generated
                        ) ([1..4] :: [Int])
{-# INLINE bench #-}


main :: IO ()
main =
  Criterion.defaultMain
    [ Criterion.bgroup "Combinators decoder nested" $
        bench "nested" (Decode.decode deeplyNestedDecoder) deeplyNestedValue
    , Criterion.bgroup "Derived (generic) decoder nested" $
        bench "nested" (Aeson.decode @ DeeplyNested) deeplyNestedValue
    , Criterion.bgroup "Implemented instance decoder nested" $
        bench "nested" (Aeson.decode @ DeeplyNested') deeplyNestedValue
    , Criterion.bgroup "Combinators decoder narrow" $
        bench "narrow" (Decode.decode narrowDecoder) narrowValue
    , Criterion.bgroup "Derived (generic) decoder narrow" $
        bench "narrow" (Aeson.decode @ Narrow) narrowValue
    , Criterion.bgroup "Implemented instance decoder narrow" $
        bench "narrow" (Aeson.decode @ Narrow') narrowValue
    ]


data DeeplyNested = DeeplyNested
    { nested :: ![DeeplyNested]
    } deriving stock (Show, Generic)
      deriving anyclass (FromJSON, NFData, ToJSON)


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


data DeeplyNested' = DeeplyNested'
    { nested' :: ![DeeplyNested']
    } deriving stock (Show, Generic)
      deriving anyclass (NFData)


instance FromJSON DeeplyNested' where
  parseJSON = withObject "DeeplyNested'" $ \v ->
    DeeplyNested' <$> v .: "nested"


data Narrow = Narrow
    { narrow :: ![Int]
    } deriving stock (Show, Generic)
      deriving anyclass (FromJSON, NFData, ToJSON)


narrowDecoder :: Decoder Narrow
narrowDecoder = Narrow
  <$> Decode.key "narrow" (Decode.list Decode.int)


narrowValue :: Int -> ByteString
narrowValue width = Aeson.encode $ Narrow [1..width]


data Narrow' = Narrow'
    { narrow' :: ![Int]
    } deriving stock (Show, Generic)
      deriving anyclass (NFData)


instance FromJSON Narrow' where
  parseJSON = withObject "narrow'" $ \v ->
    Narrow' <$> v .: "narrow"
