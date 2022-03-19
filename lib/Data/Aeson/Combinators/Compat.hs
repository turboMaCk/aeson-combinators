{-# LANGUAGE CPP #-}

module Data.Aeson.Combinators.Compat (
  -- * Re-exposes compatibility functions for aeson-2

  -- * Backwards compatibility with KeyMap
  KeyMap
  , toHashMapText
  -- * Backwards compatibility with KeyMap
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

type KeyMap a = HL.HashMap Text a
type Key = Text

-- | Aeson 2.0 compatibility function for the 'Key' type.
fromText :: Text -> Text
fromText = id

-- | Aeson 2.0 compatibility function for the 'Key' type.
toText :: Text -> Text
toText = id

toHashMapText :: HL.HashMap Text a -> HL.HashMap Text a
toHashMapText = id
#endif
