module Data.JSON.Decoder where

import qualified Data.Aeson           as Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict  as HM
import           Data.Text

newtype Decoder a =
  Decoder (Value -> Maybe a)

def :: FromJSON a => Decoder a
def = Decoder $ \v -> case fromJSON v of
            Success a -> Just a
            _ -> Nothing

text :: Decoder Text
text = Decoder $
  \v -> case v of
          String t -> Just t
          _        -> Nothing

instance Functor Decoder where
  fmap f (Decoder d) = Decoder $ fmap f . d

instance Applicative Decoder where
  pure val = Decoder $ \_ -> Just val
  (Decoder f') <*> (Decoder d) = Decoder $
    \val ->
      case f' val of
        Just f  -> fmap f (d val)
        Nothing -> Nothing


field :: Text -> Decoder a -> Decoder a
field key (Decoder d) = Decoder $
  \val -> case val of
            Object hasmap -> d =<< HM.lookup key hasmap
            _             -> Nothing

decode :: ByteString -> Decoder a -> Maybe a
decode bs (Decoder d) =
  Aeson.decode bs >>= d
