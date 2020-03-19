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

import           Data.Aeson           (ToJSON, Value)
import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Encoding  as E
import qualified Data.ByteString.Lazy as BS

type Encoder a = (a -> Value)

auto :: ToJSON a => Encoder a
auto = Aeson.toJSON

encode :: Encoder a -> a -> BS.ByteString
encode encoder = E.encodingToLazyByteString . (toEncoding encoder)

-- Private

toEncoding :: Encoder a -> a -> E.Encoding
toEncoding f = E.value . f
