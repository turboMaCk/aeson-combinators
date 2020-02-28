{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Data.Aeson.Cominators.Decode
-- Copyright   : (c) Marek Fajkus
-- License     : BSD3
--
-- Maintainer  : marek.faj@gmail.com
--
-- Aeson decoding API is closed over the type class 'FromJSON'.
-- Because of this there is one to one mapping between JSON
-- format and data decoded from it.
-- While this is handy in many situations it forces
-- users of Aeson library to define proxy types and
-- data wrappers just for sake of implementing instance
-- of 'FromJSON'.
--
module Data.Aeson.Combinators.Decode (
  -- * Example Usage
  -- $usage

  -- ** Applicative "Elm Style" Decoders
  -- $applicative
   Decoder(..)
  , auto
-- * Decoding Primitive Values
--
-- *** Boolean
  , bool
-- *** Integers
  , int, integer, int8, int16, int32, int64
#if (MIN_VERSION_base(4,8,0))
  , natural
#endif
  , version
-- *** Floating Points
  , float, double
-- *** Strings
  , char, text, string, word, word8, word16, word32, word64
  , uuid
-- * Decoding Time
  , zonedTime, localTime, timeOfDay, utcTime, day, dayOfWeek
-- * Decodeing Containers
-- *** Maybe
  , nullable
-- *** Sequences
  , list, vector
-- *** Hasmap
  , hashMapLazy, hashMapStrict
-- *** Map
  , mapLazy, mapStrict
-- * Combinators
  , jsonNull
-- *** Objects:
  , key
  , at
-- *** Arrays
  , index
-- *** Path
  , element
  , path
-- * Running Decoders
-- *** Decoding From Byte Strings
  , decode, decode'
  , eitherDecode, eitherDecode'
  , decodeStrict, decodeStrict'
  , eitherDecodeStrict, eitherDecodeStrict'
-- *** Decoding Files
  , decodeFileStrict, decodeFileStrict'
  , eitherDecodeFileStrict, eitherDecodeFileStrict'
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
import           Data.Word                  (Word, Word16, Word32, Word64,
                                             Word8)
#if (MIN_VERSION_base(4,8,0))
import           GHC.Natural                (Natural)
#endif
import qualified Data.HashMap.Lazy          as HL
import qualified Data.HashMap.Strict        as HS
import qualified Data.Map.Lazy              as ML
import qualified Data.Map.Strict            as MS
import           Data.Traversable           (traverse)
import           Prelude                    hiding (fail)

-- $usage
-- As mentioned above, combinators and type classes can be used together.
--
-- __Decode type nested in json__
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > import Data.Text
-- > import Data.ByteString.Lazy (ByteString)
-- > import Data.Aeson.Types
-- > import qualified Data.Aeson.Combinators.Decode as ACD
-- > import GHC.Generics
-- >
-- > data Person = Person
-- >     { name :: Text
-- >     , age  :: Int
-- >     } deriving (Generic, Show)
-- >
-- > instance FromJSON Person
-- >
-- > decodeEmbededPerson :: [Text] -> ByteString -> Maybe Person
-- > decodeEmbededPerson path json =
-- >     ACD.decode (ACD.at path ACD.auto) json
-- >
--
-- Now we can extract Person from any key within the json.
--
-- > >>> decodeEmbededPerson ["data", "person"] "{\"data\": {\"person\":{\"name\":\"Joe\",\"age\":12}}}"
-- > Just (Person {name = "Joe", age = 12})
--
-- __Easily decode multiple data from single json:__
--
-- > -- data Person defined above ^
-- >
-- >  type Token = Text
-- >
-- >  decodePersonWithToken :: ByteString -> Maybe (Token, Person)
-- >  decodePersonWithToken json = ACD.decode decoder json
-- >      where decoder =
-- >              (,) <$> ACD.key "token" ACD.text
-- >                  <*> ACD.key "person" ACD.auto
--
-- Which can be used as following
--
-- > >>> decodePersonWithToken "{\"person\":{\"name\":\"Joe\",\"age\":12}, \"token\": \"foo\"}"
-- > Just ("foo",Person {name = "Joe", age = 12})

-- $applicative
--
-- If you like elm style decoding you can avoid using 'FromJSON' type class all togher.
--
-- > import Data.Text
-- > import qualified Data.Aeson.Combinators.Decode as ACD
-- >
-- > data Person = Person {
-- >     name :: Text
-- >     , age  :: Int
-- >     } deriving (Show)
-- >
-- > personDecoder :: ACD.Decoder Person
-- > personDecoder = Person
-- >         <$> ACD.key "name" ACD.text
-- >         <*> ACD.key "age" ACD.int
--
-- And use it directly as:
--
-- > >>> decode personDecoder "{\"name\":\"Joe\",\"age\":12}"
-- > Just (Person {name = "Joe", age = 12})


-- | === JSON Decoder
--
-- A value that describes how values are decoded from JSON.
-- This type is an alternative to Aeson's 'FromJSON' instance implementation.
--
-- Use 'decode', 'decode', 'eitherDecode', 'eitherDecode''
-- 'decodeStrict', 'decodeStrict'', 'eitherDecodeStrict' or 'eitherDecodeStrict''
-- alternatives provided by this module for decoding from 'BytString'.
--
-- For decoding files use
-- 'decodeFileStrict', 'decodeFileStrict''
-- 'eitherDecodeFileStrict', 'eitherDecodeFileStrict''
-- also provided by this module.
--
-- ==== Using Instances of Decoder
-- __Functor to map function over 'Decoder'__
--
-- > intToString :: Decoder String
-- > intToString = show <$> Decode.int
--
-- > >>> decode intToString "2"
-- > Just "2"
--
-- __Applicateve to construct products__
--
-- > stringIntPair :: Decoder (String, Int)
-- > stringIntPair = (,) <$> index 0 string
-- >                     <*> index 1 int
--
-- > >>> decode stringIntPair "[\"hello\", 42]"
-- > Just ("hello", 42)
--
-- __Alternative to construct sums__
--
-- > eitherTextOrInt :: Decoder (Either Text Int)
-- > eitherTextOrInt = Left  <$> Decode.text
-- >               <|> Right <$> Decode.int
--
-- > >>> decode eitherTextOrInt "\"Lorem Ipsum\""
-- > Just (Left "Lorem Ipsum")
-- > >>> decode eitherTextOrInt "42"
-- > Just (Right 42)
--
-- __Monad for 'Decoder' chaining__
--
-- > odd :: Decoder Int
-- > odd = do
-- >   val <- int
-- >   if val % 2 == 1
-- >   then $ return val
-- >   else Fail.fail $ "Expected odd value, got " <> show val -- Using Control.Monad.Fail
--
-- > >>> eitherDecode odd "3"
-- > Left 3
-- > >>> eitherDecode odd "4"
-- > Right "Expected odd value, got 4"
newtype Decoder a =
  Decoder (Value -> Parser a)

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

-- Basic Decoders

-- | 'Decoder' is compatible with Aeson's 'FromJSON' class
-- 'auto' decoder acts like a proxy to instance implementation.
-- Any type that is an instance of this class is automatically compatible.
--
-- While 'auto' is universally usable for all primitive values,
-- this library provides individual type constraint functions
-- for decoding those values.
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
jsonNull a = Decoder $ \case
  Null -> pure a
  val    -> typeMismatch "null" val
{-# INLINE jsonNull #-}


-- Continer Decoders

nullable :: Decoder a -> Decoder (Maybe a)
nullable (Decoder d) = Decoder $ \case
  Null  -> pure Nothing
  other -> Just <$> d other
{-# INLINE nullable #-}

list :: Decoder a -> Decoder [a]
list (Decoder d) = Decoder $
  listParser d
{-# INLINE list #-}

vector :: Decoder a -> Decoder (Vector a)
vector (Decoder d) = Decoder $ \case
  Array v -> Vector.mapM d v
  other   -> typeMismatch "Array" other
{-# INLINE vector #-}

hashMapStrict :: Decoder a -> Decoder (HS.HashMap Text a)
hashMapStrict (Decoder d) = Decoder $ \case
  Object xs -> traverse d xs
  val -> typeMismatch "Array" val
{-|# INLINE hashMapStrict #-}

hashMapLazy :: Decoder a -> Decoder (HL.HashMap Text a)
hashMapLazy (Decoder d) = Decoder $ \case
  Object xs -> traverse d xs
  val -> typeMismatch "Array" val
{-|# INLINE hashMapLazy #-}

mapStrict :: Decoder a -> Decoder (MS.Map Text a)
mapStrict dec = MS.fromList . HL.toList <$> hashMapLazy dec
{-|# INLINE mapStrict #-}

mapLazy :: Decoder a -> Decoder (ML.Map Text a)
mapLazy dec = ML.fromList . HL.toList <$> hashMapLazy dec
{-|# INLINE mapStrict #-}


-- Object Combinators

key :: Text -> Decoder a -> Decoder a
key t (Decoder d) = Decoder $ \case
  Object v -> d =<< v .: t
  val        -> typeMismatch "Object" val
{-# INLINE key #-}

at :: [Text] -> Decoder a -> Decoder a
at pth d =
  foldr key d pth
{-# INLINE at #-}

index :: Int -> Decoder a -> Decoder a
index i (Decoder d) = Decoder $ \val ->
  case val of
    Array vec -> case vec !? i of
                    Just v  -> d v
                    Nothing -> unexpected val
    _         -> typeMismatch "Array" val
{-# INLINE index #-}

element :: JSONPathElement -> Decoder a -> Decoder a
element (Key txt) = key txt
element (Index i) = index i
{-# INLINE element #-}

path :: JSONPath -> Decoder a -> Decoder a
path pth d = foldr element d pth
{-# INLINE path #-}


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
formatError pth msg =
  "Error in " ++ formatPath pth ++ ": " ++ msg
{-# INLINE formatError #-}

eitherFormatError :: Either (JSONPath, String) a -> Either String a
eitherFormatError = either (Left . uncurry formatError) Right
{-# INLINE eitherFormatError #-}
