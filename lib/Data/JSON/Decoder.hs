module Data.JSON.Decoder where

import           Control.Monad.Fail         (MonadFail (..))
import qualified Data.Aeson.Internal        as AI
import qualified Data.Aeson.Parser          as Parser
import qualified Data.Aeson.Parser.Internal as ParserI
import           Data.Aeson.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.Text
import qualified Data.Text                  as T
import           Prelude                    hiding (fail)

newtype Decoder a =
  Decoder (Value -> Parser a)

def :: FromJSON a => Decoder a
def = Decoder parseJSON
{-# INLINE def #-}

parseChar :: Text -> Parser Char
parseChar t =
  if T.compareLength t 1 == EQ then
    pure $ T.head t
  else
    fail "expected a string of length 1"
{-# INLINE parseChar #-}

char :: Decoder Char
char = Decoder $ withText "Char" parseChar
{-# INLINE char #-}

text :: Decoder Text
text = Decoder $ withText "String" pure
{-# INLINE text #-}

instance Functor Decoder where
  fmap f (Decoder d) = Decoder $ fmap f . d
  {-# INLINE fmap #-}

instance Applicative Decoder where
  pure val = Decoder $ \_ -> pure val
  {-# INLINE pure #-}
  (Decoder f') <*> (Decoder d) = Decoder $
    \val ->
        (\f -> fmap f (d val)) =<< f' val
  {-# INLINE (<*>) #-}

instance Monad Decoder where
  (Decoder a) >>= f = Decoder $
    \val -> case parse a val of
      Success v -> let (Decoder res) = f v
                   in res val
      _ -> unexpected val
  {-# INLINE (>>=) #-}

instance MonadFail Decoder where
  fail s = Decoder $ \_ -> fail s

field :: Text -> Decoder a -> Decoder a
field t (Decoder d) = Decoder $
  \val -> case val of
    Object v -> d =<< v .: t
    _        -> typeMismatch "Object" val
{-# INLINE field #-}

list :: Decoder a -> Decoder [a]
list (Decoder f) = Decoder $
  listParser f
{-# INLINE list #-}

decode :: Decoder a -> LB.ByteString -> Maybe a
decode (Decoder f) =
  Parser.decodeWith ParserI.jsonEOF (parse f)
{-# INLINE decode #-}

decode' :: Decoder a -> LB.ByteString -> Maybe a
decode' (Decoder f) =
  Parser.decodeWith ParserI.jsonEOF' (parse f)
{-# INLINE decode' #-}

-- | Annotate an error message with a
-- <http://goessner.net/articles/JsonPath/ JSONPath> error location.
formatError :: JSONPath -> String -> String
formatError path msg =
  "Error in " ++ formatPath path ++ ": " ++ msg
{-# INLINE formatError #-}

eitherFormatError :: Either (JSONPath, String) a -> Either String a
eitherFormatError = either (Left . uncurry formatError) Right
{-# INLINE eitherFormatError #-}

eitherDecode :: Decoder a -> LB.ByteString -> Either String a
eitherDecode (Decoder f) =
  eitherFormatError . Parser.eitherDecodeWith ParserI.jsonEOF (AI.iparse f)
{-# INLINE eitherDecode #-}

eitherDecode' :: Decoder a -> LB.ByteString -> Either String a
eitherDecode' (Decoder f) =
  eitherFormatError . Parser.eitherDecodeWith ParserI.jsonEOF' (AI.iparse f)
{-# INLINE eitherDecode' #-}

-- Strict

decodeStrict :: Decoder a -> B.ByteString -> Maybe a
decodeStrict (Decoder f) =
  Parser.decodeStrictWith ParserI.jsonEOF (parse f)
{-# INLINE decodeStrict #-}

decodeStrict' :: Decoder a -> B.ByteString -> Maybe a
decodeStrict' (Decoder f) =
  Parser.decodeStrictWith ParserI.jsonEOF' (parse f)
{-# INLINE decodeStrict' #-}
