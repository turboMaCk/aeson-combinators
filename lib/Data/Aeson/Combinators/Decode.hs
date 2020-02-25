{-# LANGUAGE CPP #-}

module Data.Aeson.Combinators.Decode
  ( Decoder(..)
  , auto
  , bool
  , int
  , int8
  , int16
  , int32
  , int64
  , integer
#if (MIN_VERSION_base(4,8,0))
  , natural
#endif
  , char
  , word
  , word8
  , word16
  , word32
  , word64
  , text
  , string
  , float
  , double
  , version
  , zonedTime
  , localTime
  , timeOfDay
  , utcTime
  , day
  , dayOfWeek
  , uuid
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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail         (MonadFail (..))
import qualified Control.Monad.Fail         as Fail
import qualified Data.Aeson.Internal        as AI
import qualified Data.Aeson.Parser          as Parser
import qualified Data.Aeson.Parser.Internal as ParserI
import           Data.Aeson.Types
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as LB
import           Data.Int                   (Int16, Int32, Int64, Int8)
import           Data.Text                  (Text)
import           Data.Time.Calendar         (Day)
import           Data.Time.Calendar.Compat  (DayOfWeek)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.LocalTime        (LocalTime, TimeOfDay, ZonedTime)
import           Data.UUID.Types            (UUID)
import           Data.Vector                (Vector, (!?))
import qualified Data.Vector                as Vector
import           Data.Version               (Version)
import           Data.Word                  (Word16, Word32, Word64, Word8, Word)
#if (MIN_VERSION_base(4,8,0))
import           GHC.Natural                (Natural)
#endif
import           Prelude                    hiding (fail)

newtype Decoder a =
  Decoder (Value -> Parser a)

-- Basic Decoders

auto :: FromJSON a => Decoder a
auto = Decoder parseJSON
{-# INLINE auto #-}

bool :: Decoder Bool
bool = auto
{-# INLINE bool #-}

char :: Decoder Char
char = auto
{-# INLINE char #-}

string :: Decoder String
string = auto
{-# INLINE string #-}

text :: Decoder Text
text = auto
{-# INLINE text #-}

int :: Decoder Int
int = auto
{-# INLINE int #-}

int8 :: Decoder Int8
int8 = auto
{-# INLINE int8 #-}

int16 :: Decoder Int16
int16 = auto
{-# INLINE int16 #-}

int32 :: Decoder Int32
int32 = auto
{-# INLINE int32 #-}

int64 :: Decoder Int64
int64 = auto
{-# INLINE int64 #-}

integer :: Decoder Integer
integer = auto
{-# INLINE integer #-}

#if (MIN_VERSION_base(4,8,0))
natural :: Decoder Natural
natural = auto
{-# INLINE natural #-}
#endif

float :: Decoder Float
float = auto
{-# INLINE float #-}

double :: Decoder Double
double = auto
{-# INLINE double #-}

word :: Decoder Word
word = auto
{-# INLINE word #-}

word8 :: Decoder Word8
word8 = auto
{-# INLINE word8 #-}

word16 :: Decoder Word16
word16 = auto
{-# INLINE word16 #-}

word32 :: Decoder Word32
word32 = auto
{-# INLINE word32 #-}

word64 :: Decoder Word64
word64 = auto
{-# INLINE word64 #-}

version :: Decoder Version
version = auto
{-# INLINE version #-}

zonedTime :: Decoder ZonedTime
zonedTime = auto
{-# INLINE zonedTime #-}

localTime :: Decoder LocalTime
localTime = auto
{-# INLINE localTime #-}

timeOfDay :: Decoder TimeOfDay
timeOfDay = auto
{-# INLINE timeOfDay #-}

utcTime :: Decoder UTCTime
utcTime = auto
{-# INLINE utcTime #-}

day :: Decoder Day
day = auto
{-# INLINE day #-}

dayOfWeek :: Decoder DayOfWeek
dayOfWeek = auto
{-# INLINE dayOfWeek #-}

uuid :: Decoder UUID
uuid = auto
{-# INLINE uuid #-}

jsonNull :: a -> Decoder a
jsonNull a = Decoder $ \val ->
  case val of
    Null -> pure a
    _    -> typeMismatch "null" val
{-# INLINE jsonNull #-}


-- Continer Decoders

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


-- Instances

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
#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance Alternative Decoder where
  empty = Decoder unexpected
  {-# INLINE empty #-}
  Decoder a <|> Decoder b = Decoder $ \v -> a v <|> b v
  {-# INLINE (<|>) #-}

instance MonadFail Decoder where
  fail s = Decoder $ \_ -> Fail.fail s
  {-# INLINE fail #-}


-- Object Combinators

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


-- File Decoding

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
