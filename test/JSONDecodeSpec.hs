{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module JSONDecodeSpec where

import           Data.Aeson.Types     (FromJSON (..))
import           Data.ByteString.Lazy
import qualified Data.JSON.Decoder    as JD
import           Data.Text
import           GHC.Generics
import           Test.Hspec
import Control.Exception

data Object =
  Object
  { name :: Text
  , nick :: Text
  } deriving (Show, Eq, Generic)

data FooBar
  = Foo
  | Bar
  deriving (Show, Eq)

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
        JD.decode decoder json `shouldBe` res

      it "should decode nested object" $ do
        JD.decode (JD.field "data" decoder) jsonNested
          `shouldBe` res

      it "should be possible to use default decoder" $ do
        JD.decode JD.def json `shouldBe` res

    context "monadic decoding" $ do
      let fromText v =
            case v of
              "foo" -> pure Foo
              "bar" -> pure Bar
              _     -> fail "unknown"
      it "should work as a dummy value" $ do
        JD.decode (JD.text >>= pure) "\"foo\""
          `shouldBe` (Just "foo")

      it "should turn string to sum" $ do
        JD.decode (JD.text >>= fromText) "\"foo\""
          `shouldBe` (Just Foo)

      it "should fail with right error" $ do
        evaluate (JD.decode (JD.text >>= fromText) "\"foobar\"")
          `shouldThrow` (errorCall "unknown")
