{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSONDecodeSpec where

import           Control.Applicative
import qualified Data.Aeson.Combinators.Decode as JD
import           Data.Aeson.Types              (FromJSON (..))
import           Data.ByteString.Lazy
import           Data.ByteString.Lazy.UTF8     (fromString)
import           Data.Monoid                   ((<>))
import           Data.Text
import           GHC.Generics
import           Test.Hspec

data Object = Object
    { name :: Text
    , nick :: Text
    }
    deriving (Show, Eq, Generic)

data FooBar = Foo | Bar
    deriving (Show, Eq)

instance FromJSON Object

decoder :: JD.Decoder Object
decoder =
  Object
    <$> JD.key "name" JD.text
    <*> JD.key "nick" JD.text

json :: ByteString
json = "{\"name\":\"Jany Doe\",\"nick\": \"jany\"}"

jsonNested :: ByteString
jsonNested = "{\"data\":" <> json <> "}"

objectSpec :: Spec
objectSpec = do
  describe "field" $ do
    let res = Just $ Object "Jany Doe" "jany"

    it "should decode object" $ do
      JD.decode decoder json `shouldBe` res

    it "should decode nested object" $ do
      JD.decode (JD.key "data" decoder) jsonNested
        `shouldBe` res

    it "should be possible to use default decoder" $ do
      JD.decode JD.auto json `shouldBe` res

  describe "at" $ do
    let
      nest' :: Int -> ByteString -> ByteString
      nest' n inner =
          if n <= 0 then
            inner
          else
            nest' (n - 1) ("{\"level-" <> fromString (show n) <> "\":" <> inner <> "}")
    let nest n = nest' n json
    let res = Just $ Object "Jany Doe" "jany"

    it "should decode object directly for empty list" $ do
      JD.decode (JD.at [] decoder) json
       `shouldBe` res

    it "should decode 2 level json" $ do
      let jdata = nest 2

      JD.decode (JD.at ["level-1", "level-2"] decoder) jdata
       `shouldBe` res

    it "should decode 10 level json" $ do
      let jdata = nest 10
      let path = ["level-1", "level-2", "level-3", "level-4", "level-5"
             , "level-6", "level-7", "level-8", "level-9", "level-10"
             ]

      JD.decode (JD.at path decoder) jdata `shouldBe` res

arraySpec :: Spec
arraySpec = do
  describe "index" $ do
    it "decode singleton by index" $ do
      JD.decode (JD.index 0 decoder) "[{\"name\": \"Sanders\", \"nick\": \"bern\"}]"
        `shouldBe` Just (Object "Sanders" "bern")

    it "should decode nth index" $ do
      JD.decode (JD.index 1 decoder) "[false, {\"name\": \"Sanders\", \"nick\": \"bern\"}, true]"
        `shouldBe` Just (Object "Sanders" "bern")

monadSpec :: Spec
monadSpec =
  describe "monadic decoding" $ do
    let fromText v =
          case v of
            "foo" -> pure Foo
            "bar" -> pure Bar
            _     -> fail "unknown"

    it "should work as a dummy value" $ do
      JD.decode (JD.string >>= pure) "\"foo\""
        `shouldBe` Just "foo"

    it "should turn string to sum" $ do
      JD.decode (JD.string >>= fromText) "\"foo\""
        `shouldBe` Just Foo

    it "should fail with right error" $ do
      JD.eitherDecode (JD.string >>= fromText) "\"foobar\""
        `shouldBe` Left "Error in $: unknown"

alternativeSpec :: Spec
alternativeSpec =
  describe "alternative (<|>)" $ do
    let (dec :: JD.Decoder (Either Bool Object)) =
          Left <$> JD.bool <|> Right <$> decoder

    it "should decode first alternative" $ do
      JD.decode dec "false" `shouldBe` Just (Left False)

    it "should decode second alternative" $ do
      JD.decode dec "{\"name\": \"Joe\",\"nick\": \"jd\"}"
        `shouldBe` Just (Right $ Object "Joe" "jd")

jsonNullSpec :: Spec
jsonNullSpec =
  describe "jsonNull" $ do
    let barDec txt =
          case txt of
            "bar" -> pure Bar
            _     -> fail $ "Unknown value" <> show txt

    let (dec :: JD.Decoder FooBar) =
          JD.jsonNull Foo <|> (barDec =<< JD.text)

    it "should decode Foo from null" $ do
      JD.decode dec "null" `shouldBe` Just Foo

    it "should decode Bar from string" $ do
      JD.decode dec "\"bar\"" `shouldBe` Just Bar

spec :: Spec
spec = do
  objectSpec
  arraySpec
  monadSpec
  alternativeSpec
  jsonNullSpec
