{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.Aeson.Cominators.Encode
-- Copyright   : (c) Marek Fajkus
-- License     : BSD3
--
-- Maintainer  : marek.faj@gmail.com
--
-- TODO: document
--
module Data.Aeson.Combinators.Encode where

import           Data.Aeson                 (ToJSON, Value)
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encoding        as E
import qualified Data.ByteString.Lazy       as BS
import           Data.Functor.Contravariant
import           Data.Text                  (Text)


newtype Encoder a = Encoder (a -> Value)

auto :: ToJSON a => Encoder a
auto = Encoder Aeson.toJSON

instance Contravariant Encoder where
  contramap f (Encoder enc) = Encoder (enc . f)

type KeyValueEncoder a = a -> (Text, Value)

field :: Text -> Encoder b -> (a -> b) -> KeyValueEncoder a
field name (Encoder enc) get = \v -> (name, enc $ get v)

object :: [KeyValueEncoder a] -> Encoder a
object xs = Encoder $ \val -> Aeson.object $ (\f -> f val) <$> xs

type KeyValuesEncoder' a = a -> [(Text, Value)]

field' :: Text -> Encoder a -> a -> (Text, Value)
field' name (Encoder enc) val = (name, enc val)

object' :: KeyValuesEncoder' a -> Encoder a
object' f = Encoder $ \val -> Aeson.object $ f val

-- Combinators


-- Encode

encode :: Encoder a -> a -> BS.ByteString
encode encoder = E.encodingToLazyByteString . (toEncoding encoder)

-- Private

toEncoding :: Encoder a -> a -> E.Encoding
toEncoding (Encoder enc) = E.value . enc
