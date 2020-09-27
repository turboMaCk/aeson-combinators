{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.Aeson.Cominators.Encode
-- Copyright   : (c) Marek Fajkus
-- License     : BSD3
--
-- Maintainer  : marek.faj@gmail.com
--
-- Functions in this module serve as an alternative
-- 'ToJSON' type class. This allows to define for mapping
-- from data type into multiple JSON representations.
-- type level wrapping.
--
-- There are two way of defining such encoder:
--
--     * Using simple function @a -> Value@ which doesn't require this library
--     * Using this library as DSL together with 'Contravariant'
--
module Data.Aeson.Combinators.Encode (
  -- * Example Usage
  -- $usage

    Encoder(..)
  , auto
  , field
  , object
  , field'
  , object'
  , vector
  , list
  , encode
  , toEncoding
) where

import           Control.Applicative
import           Data.Aeson                           (ToJSON, Value (..))
import qualified Data.Aeson                           as Aeson
import qualified Data.Aeson.Encoding                  as E
import qualified Data.ByteString.Lazy                 as BS
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Text                            (Text)
import           Data.Vector                          (Vector, fromList)
import qualified Data.Vector                          as Vector
import           Data.Void                            (absurd)

{- $usage
This module as meant to be import as @qualified@

> import Data.Aeson.Combinators.Encode as Encode

__Decode type nested in json__
>>> :set -XOverloadedStrings
>>> :set -XDeriveGeneric

some shit here

>>> :{
data Object = Object
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)
:}

>>> :{
objectEncoder :: Encoder Object
objectEncoder = object
  [ field "name" auto name
  , field "age" auto age
  ]
:}

>>> encode objectEncoder $ Object "Jane" 42
"{\"age\":42,\"name\":\"Jane\"}"
-}


newtype Encoder a = Encoder (a -> Value)


auto :: ToJSON a => Encoder a
auto = Encoder Aeson.toJSON


instance Contravariant Encoder where
  contramap f (Encoder enc) = Encoder (enc . f)


instance Divisible Encoder where
  conquer = Encoder (const Null)

  divide toPair (Encoder encA) (Encoder encB) = Encoder $ \val ->
    case toPair val of
      (a, b) -> Array $ Vector.fromList [ encA a, encB b ]


instance Decidable Encoder where
  lose f = Encoder $ absurd . f

  choose split (Encoder encL) (Encoder encR) =
      Encoder $ \val ->
          case split val of
            Left l  -> encL l
            Right r -> encR r


-- Combinators


type KeyValueEncoder a = a -> (Text, Value)


field :: Text -> Encoder b -> (a -> b) -> KeyValueEncoder a
field name (Encoder enc) get = \v -> (name, enc $ get v)


object :: [KeyValueEncoder a] -> Encoder a
object xs = Encoder $ \val -> Aeson.object $ fmap (\f -> f val) xs


type KeyValuesEncoder' a = a -> [(Text, Value)]


field' :: Text -> Encoder a -> a -> (Text, Value)
field' name (Encoder enc) val = (name, enc val)


object' :: KeyValuesEncoder' a -> Encoder a
object' f = Encoder $ \val -> Aeson.object $ f val


vector :: Encoder a -> Encoder (Vector a)
vector (Encoder f) = Encoder $ \val -> Aeson.Array $ f <$> val


list :: Encoder a -> Encoder [a]
list (Encoder f) = Encoder $ \val -> Aeson.Array $ fromList $ f <$> val


-- Encode


encode :: Encoder a -> a -> BS.ByteString
encode encoder = E.encodingToLazyByteString . (toEncoding encoder)


-- Private

toEncoding :: Encoder a -> a -> E.Encoding
toEncoding (Encoder enc) = E.value . enc
