{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module JSONEncodeSpec where

import qualified Data.Aeson.Combinators.Encode as JE
import           Data.Text
import           Test.Hspec


data Object = Object
    { name :: Text
    , age  :: Int
    }
    deriving (Show, Eq)


objectEncoder :: JE.Encoder Object
objectEncoder = JE.object
  [ JE.field "name" JE.auto name
  , JE.field "age" JE.auto age
  ]


objectEncoder' :: JE.Encoder Object
objectEncoder' = JE.object' $ \Object {..} ->
  [ JE.field' "name" JE.auto name
  , JE.field' "age" JE.auto age
  ]


encodePrimitives :: Spec
encodePrimitives = describe "primitives" $ do
  it "encode bool" $ do
    JE.encode JE.auto True `shouldBe` "true"


objectEncoding :: Spec
objectEncoding = do
  let object = Object "Joe" 30
  let json = "{\"age\":30,\"name\":\"Joe\"}"

  describe "object encoding" $ do
    it "should encode using getter style encoding" $ do
      JE.encode objectEncoder object `shouldBe` json

    it "should encode using explicit style encoding" $ do
      JE.encode objectEncoder' object `shouldBe` json


listSpec :: Spec
listSpec = describe "list encoding" $ do
  it "encodes list of bools" $ do
    JE.encode (JE.list JE.auto) [True, False] `shouldBe` "[true,false]"


data MyRec = MyRec
  { recTitle :: String
  , recStart :: Int
  , recEnd   :: Int
  } deriving (Show, Eq)


spec :: Spec
spec = do
  encodePrimitives
  objectEncoding
  listSpec
