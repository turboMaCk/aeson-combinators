{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONDecodeSpec where

import           Data.Aeson.Types     (FromJSON (..))
import           Data.ByteString.Lazy
import qualified Data.JSON.Decoder    as JD
import           Data.Text
import           Test.Hspec
import GHC.Generics

data Object =
  Object
  { name :: Text
  , nick :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Object

decoder :: JD.Decoder Object
decoder =
  Object
    <$> JD.field "name" JD.text
    <*> JD.field "nick" JD.def

json :: ByteString
json = "{\"name\":\"Jany Doe\",\"nick\": \"jany\"}"

jsonNested :: ByteString
jsonNested = "{\"data\":" <> json <> "}"

spec :: Spec
spec = do
    context "simple object" $ do
      let res = Just $ Object "Jany Doe" "jany"
      it "should decode object" $ do
        JD.decode json decoder `shouldBe` res
      it "should decode nested object" $ do
        JD.decode jsonNested (JD.field "data" decoder)
          `shouldBe` res
      it "should be possible to use default decoder" $ do
        JD.decode json JD.def `shouldBe` res
