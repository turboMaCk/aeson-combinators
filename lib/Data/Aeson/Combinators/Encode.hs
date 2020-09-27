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
  -- * Importing
  -- $importing

  -- * Alternative
  -- $alternative

  -- * Example Usage
  -- $usage

  -- * Encoder
    Encoder(..)
  , auto
  , run
  -- * Object Encoding
  , KeyValueEncoder
  , field
  , object
  -- * Alternative Object Encoding
  , KeyValueEncoder'
  , field'
  , object'
  -- * Collection Encoding
  , vector
  , list
  -- * Evaluating Encoders
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

{- $importing
This module as meant to be import as @qualified@

> import Data.Aeson.Combinators.Encode as Encode
-}

{- $alternative
Be aware than in most cause you won't need to use this module.
you can utilize Aeson's 'Value' type and it's instance of 'ToJSON' directly.

>>> import qualified Data.Aeson as Aeson
>>> import Data.Aeson ((.=))

>>> data Object = Object { tag :: String, id :: Int }

Define custom encoding function:

>>> :{
encodeObject :: Object -> Value
encodeObject (Object tag id) =
        Aeson.object ["tag" .= tag, "id" .= id]
:}

>>> Aeson.encode (encodeObject (Object "foo" 42))
"{\"tag\":\"foo\",\"id\":42}"
-}

{- $usage

>>> :set -XOverloadedStrings
>>> :set -XDeriveGeneric

First lets define some type

>>> :{
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)
:}

>>> :{
personEncoder :: Encoder Person
personEncoder = object
  [ field "name" auto name
  , field "age" auto age
  ]
:}

>>> encode personEncoder (Person "Jane" 42)
"{\"age\":42,\"name\":\"Jane\"}"

Now we can use 'Contravariant' to manipulate our encoder.

Our type might be wrap in some rither type like this one:

>>> import Data.Functor.Contravariant
>>> data Surrounding = S Person Bool

But we still want to be able to encode it:

>>> :{
surroundingEncoder :: Encoder Surrounding
surroundingEncoder = contramap (\(S person _) -> person) personEncoder
:}

>>> encode surroundingEncoder (S (Person "Joe" 24) False)
"{\"age\":24,\"name\":\"Joe\"}"

Or perhaps we want to encode 'Surronding' structure including 'Bool' as well:

>>> :{
pairEncoder :: Encoder Surrounding
pairEncoder = contramap (\(S person bool) -> (run personEncoder person, bool)) auto
:}

>>> encode pairEncoder (S (Person "Joe" 24) True)
"[{\"age\":24,\"name\":\"Joe\"},true]"

-}


newtype Encoder a = Encoder (a -> Value)

run :: Encoder a -> a -> Value
run (Encoder f) a = f a

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


type KeyValueEncoder' a = a -> [(Text, Value)]


field' :: Text -> Encoder a -> a -> (Text, Value)
field' name (Encoder enc) val = (name, enc val)


object' :: KeyValueEncoder' a -> Encoder a
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
