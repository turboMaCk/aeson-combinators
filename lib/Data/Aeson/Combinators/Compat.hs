{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Aeson.Combinators.Decode
-- Copyright   : (c) Marek Fajkus
-- License     : BSD3
--
-- Maintainer  : marek.faj@gmail.com
--
-- Aeson compatibility layer to support Aeson 2.0 and older versions.
-- Re-exposes 'Key' and 'KeyMap', together with suitable conversion functions.
-- For older aeson versions, we provide type definitions for 'Key' and
-- 'KeyMap'.
--
-- Users may use 'fromText' and 'toText' to write decoders/encoders for
-- forwards and backwards compatibility.
--
module Data.Aeson.Combinators.Compat (
-- * Aeson compatibility helpers
--
-- $doc
--
-- ** KeyMap
  KeyMap
  , toHashMapText
-- ** Key
  , Key
  , toText, fromText
) where

#if (MIN_VERSION_aeson(2,0,0))
import           Data.Aeson.Key             (Key, toText, fromText)
import           Data.Aeson.KeyMap          (KeyMap)
import           Data.Aeson.KeyMap          (toHashMapText)
#else
import qualified Data.HashMap.Lazy          as HL
import           Data.Text                  (Text)

-- | Forward compatible type-def for Aeson 2.0 'KeyMap' type.
type KeyMap a = HL.HashMap Text a

toHashMapText :: HL.HashMap Text a -> HL.HashMap Text a
toHashMapText = id

-- | Forward compatible type-def for Aeson 2.0 'Key' type.
type Key = Text

-- | Aeson 2.0 compatibility function for the 'Key' type.
fromText :: Text -> Text
fromText = id

-- | Aeson 2.0 compatibility function for the 'Key' type.
toText :: Text -> Text
toText = id
#endif

-- $doc
-- Aeson compatibility layer to support Aeson 2.0 and older versions.
-- Re-exposes 'Key' and 'KeyMap', together with suitable conversion functions.
-- For older aeson versions, we provide type definitions for 'Key' and
-- 'KeyMap'.
--
-- Users may use 'fromText' and 'toText' to write decoders/encoders for
-- forwards and backwards compatibility.
--
-- See [Key](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson-Key.html)
-- and [KeyMap](https://hackage.haskell.org/package/aeson-2.0.3.0/docs/Data-Aeson-KeyMap.html)
-- in [aeson >= 2.0](https://hackage.haskell.org/package/aeson) for more details.
