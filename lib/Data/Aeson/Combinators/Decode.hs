module Data.Aeson.Combinators.Decode
  ( Decoder(..)
  , auto
  , jsonNull
  , nullable
  , list
  , vector
  , index
  , field
  , at
  , decode
  , decode'
  , eitherDecode
  , eitherDecode'
  , decodeStrict
  , decodeStrict'
  , eitherDecodeStrict
  , eitherDecodeStrict'
  , decodeFileStrict
  , decodeFileStrict'
  , eitherDecodeFileStrict
  , eitherDecodeFileStrict'
  ) where

import           Control.Monad              hiding (fail)
import           Control.Monad.Fail         (MonadFail (..))
import           Control.Applicative
import qualified Data.Aeson.Internal        as AI
import qualified Data.Aeson.Parser          as Parser
import qualified Data.Aeson.Parser.Internal as ParserI
import           Data.Aeson.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.Text                  (Text)
import           Data.Vector                (Vector, (!?))
import qualified Data.Vector                as Vector
import           Prelude                    hiding (fail)

newtype Decoder a =
  Decoder (Value -> Parser a)

-- Basic Decoders

auto :: FromJSON a => Decoder a
auto = Decoder parseJSON
{-# INLINE auto #-}

jsonNull :: a -> Decoder a
jsonNull a = Decoder $ \val ->
  case val of
    Null -> pure a
    _    -> typeMismatch "null" val

nullable :: Decoder a -> Decoder (Maybe a)
nullable (Decoder d) = Decoder $ \val ->
  case val of
    Null  -> pure Nothing
    other -> Just <$> d other
{-# INLINE nullable #-}

list :: Decoder a -> Decoder [a]
list (Decoder d) = Decoder $
  listParser d
{-# INLINE list #-}

vector :: Decoder a -> Decoder (Vector a)
vector (Decoder d) = Decoder $ \val ->
  case val of
    Array v -> Vector.mapM d v
    other   -> typeMismatch "array" other
{-# INLINE vector #-}

index :: Int -> Decoder a -> Decoder a
index i (Decoder d) = Decoder $ \val ->
  case val of
    Array vec -> case vec !? i of
                   Just v  -> d v
                   Nothing -> unexpected val
    _         -> typeMismatch "Array" val
{-# INLINE index #-}

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

instance Alternative Decoder where
  empty = Decoder unexpected
  {-# INLINE empty #-}
  Decoder a <|> Decoder b = Decoder $ \v -> a v <|> b v
  {-# INLINE (<|>) #-}

instance MonadFail Decoder where
  fail s = Decoder $ \_ -> fail s
  {-# INLINE fail #-}

field :: Text -> Decoder a -> Decoder a
field t (Decoder d) = Decoder $
  \val -> case val of
    Object v -> d =<< v .: t
    _        -> typeMismatch "Object" val
{-# INLINE field #-}

at :: [Text] -> Decoder a -> Decoder a
at path d =
  foldr field d path
{-# INLINE at #-}


-- Decoding

decode :: Decoder a -> LB.ByteString -> Maybe a
decode (Decoder d) =
  Parser.decodeWith ParserI.jsonEOF (parse d)
{-# INLINE decode #-}

decode' :: Decoder a -> LB.ByteString -> Maybe a
decode' (Decoder d) =
  Parser.decodeWith ParserI.jsonEOF' (parse d)
{-# INLINE decode' #-}

eitherDecode :: Decoder a -> LB.ByteString -> Either String a
eitherDecode (Decoder d) =
  eitherFormatError . Parser.eitherDecodeWith ParserI.jsonEOF (AI.iparse d)
{-# INLINE eitherDecode #-}

eitherDecode' :: Decoder a -> LB.ByteString -> Either String a
eitherDecode' (Decoder d) =
  eitherFormatError . Parser.eitherDecodeWith ParserI.jsonEOF' (AI.iparse d)
{-# INLINE eitherDecode' #-}


-- Strict Decoding

decodeStrict :: Decoder a -> B.ByteString -> Maybe a
decodeStrict (Decoder d) =
  Parser.decodeStrictWith ParserI.jsonEOF (parse d)
{-# INLINE decodeStrict #-}

decodeStrict' :: Decoder a -> B.ByteString -> Maybe a
decodeStrict' (Decoder d) =
  Parser.decodeStrictWith ParserI.jsonEOF' (parse d)
{-# INLINE decodeStrict' #-}

eitherDecodeStrict :: Decoder a -> B.ByteString -> Either String a
eitherDecodeStrict (Decoder d) =
  eitherFormatError . Parser.eitherDecodeStrictWith ParserI.jsonEOF (AI.iparse d)
{-# INLINE eitherDecodeStrict #-}

eitherDecodeStrict' :: Decoder a -> B.ByteString -> Either String a
eitherDecodeStrict' (Decoder d) =
  eitherFormatError . Parser.eitherDecodeStrictWith ParserI.jsonEOF' (AI.iparse d)
{-# INLINE eitherDecodeStrict' #-}

-- Files Decoding

decodeFileStrict :: Decoder a -> FilePath -> IO (Maybe a)
decodeFileStrict dec =
  fmap (decodeStrict dec) . B.readFile
{-# INLINE decodeFileStrict #-}

decodeFileStrict' :: Decoder a -> FilePath -> IO (Maybe a)
decodeFileStrict' dec =
  fmap (decodeStrict' dec) . B.readFile
{-# INLINE decodeFileStrict' #-}

eitherDecodeFileStrict :: Decoder a -> FilePath -> IO (Either String a)
eitherDecodeFileStrict dec =
  fmap (eitherDecodeStrict dec) . B.readFile
{-# INLINE eitherDecodeFileStrict #-}

eitherDecodeFileStrict' :: Decoder a -> FilePath -> IO (Either String a)
eitherDecodeFileStrict' dec =
  fmap (eitherDecodeStrict' dec) . B.readFile
{-# INLINE eitherDecodeFileStrict' #-}


-- Private functions Aeson doesn't expose

-- | Annotate an error message with a
-- <http://goessner.net/articles/JsonPath/ JSONPath> error location.
formatError :: JSONPath -> String -> String
formatError path msg =
  "Error in " ++ formatPath path ++ ": " ++ msg
{-# INLINE formatError #-}

eitherFormatError :: Either (JSONPath, String) a -> Either String a
eitherFormatError = either (Left . uncurry formatError) Right
{-# INLINE eitherFormatError #-}
