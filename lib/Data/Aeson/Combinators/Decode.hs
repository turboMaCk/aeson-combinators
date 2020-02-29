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
-- This module provides value level 'Decoder' which can be used
-- instead of instance implementation.
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
-- *** Void, Unit, Bool
  , void
  , unit, bool
-- *** Integers (and Natural)
  , int, integer, int8, int16, int32, int64
  , word, word8, word16, word32, word64
#if (MIN_VERSION_base(4,8,0))
  , natural
#endif
-- *** Floating Points
  , float, double
  , scientific
-- *** Strings
  , char, text, string
  , uuid, version
-- * Decoding Time
  , zonedTime, localTime, timeOfDay
  , utcTime
  , day, dayOfWeek
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
  , indexes
-- *** Path
-- $jsonpath
  , element
  , path
-- * Running Decoders
-- $running
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
import           Control.Monad              hiding (void)
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
import           Data.Void                  (Void)
import           Data.Word                  (Word, Word16, Word32, Word64,
                                             Word8)
#if (MIN_VERSION_base(4,8,0))
import           GHC.Natural                (Natural)
#endif
import qualified Data.HashMap.Lazy          as HL
import qualified Data.HashMap.Strict        as HS
import qualified Data.Map.Lazy              as ML
import qualified Data.Map.Strict            as MS
import           Data.Scientific            (Scientific)
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
-- > data Person = Person
-- >     { name :: Text
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
-- >   if val `mod` 2 == 1
-- >   then $ return val
-- >   else fail $ "Expected odd value, got " <> show val
--
-- > >>> eitherDecode odd "3"
-- > Left 3
-- > >>> eitherDecode odd "4"
-- > Right "Error in $: Expected odd value, got 4"
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

-- | 'Decoder' is compatible with Aeson's 'FromJSON' class.
-- 'auto' decoder acts like a proxy to instance implementation.
-- Any type that is an instance of this class is automatically compatible.
--
-- While 'auto' is universally usable for all primitive values,
-- this library provides individual type constraint functions
-- for decoding those values.
auto :: FromJSON a => Decoder a
auto = Decoder parseJSON
{-# INLINE auto #-}

-- | Decode any JSON value to 'Void' value
-- which is impossible to construct.
--
-- __This Decoder is guarenteed to fail.__
void :: Decoder Void
void = auto
{-# INLINE void #-}

-- | Decode JSON null into '()'
unit :: Decoder ()
unit = auto
{-# INLINE unit #-}

-- | Decode JSON booleans to Haskell 'Data.Bool'
bool :: Decoder Bool
bool = auto
{-# INLINE bool #-}

-- | Decode JSON number to 'Data.Int.Int'
int :: Decoder Int
int = auto
{-# INLINE int #-}

-- | Decode JSON number to 'Data.Int.Int8'
int8 :: Decoder Int8
int8 = auto
{-# INLINE int8 #-}

-- | Decode JSON number to 'Data.Int.Int16'
int16 :: Decoder Int16
int16 = auto
{-# INLINE int16 #-}

-- | Decode JSON number to 'Data.Int.Int32'
int32 :: Decoder Int32
int32 = auto
{-# INLINE int32 #-}

-- | Decode JSON number to 'Data.Int.Int64'
int64 :: Decoder Int64
int64 = auto
{-# INLINE int64 #-}

-- | Decode JSON number to unbounded 'Integer'
integer :: Decoder Integer
integer = auto
{-# INLINE integer #-}

#if (MIN_VERSION_base(4,8,0))
-- | Decode JSON number to GHC's 'GHC.Natural' (non negative)
--
-- This function requires 'base' >= 4.8.0
natural :: Decoder Natural
natural = auto
{-# INLINE natural #-}
#endif

-- | Decode JSON number to bounded 'Data.Word.Word'
word :: Decoder Word
word = auto
{-# INLINE word #-}

-- | Decode JSON number to bounded 'Data.Word.Word8'
word8 :: Decoder Word8
word8 = auto
{-# INLINE word8 #-}

-- | Decode JSON number to bounded 'Data.Word.Word16'
word16 :: Decoder Word16
word16 = auto
{-# INLINE word16 #-}

-- | Decode JSON number to bounded 'Data.Word.Word32'
word32 :: Decoder Word32
word32 = auto
{-# INLINE word32 #-}

-- | Decode JSON number to bounded 'Data.Word.Word64'
word64 :: Decoder Word64
word64 = auto
{-# INLINE word64 #-}


-- | Decode JSON number to 'Float'
float :: Decoder Float
float = auto
{-# INLINE float #-}

-- | Decode JSON number to 'Double'
double :: Decoder Double
double = auto
{-# INLINE double #-}

-- | Decode JSON number to arbitrary precision 'Scientific'
scientific :: Decoder Scientific
scientific = auto
{-# INLINE scientific #-}

-- | Decode single character JSON string to 'Data.Char'
char :: Decoder Char
char = auto
{-# INLINE char #-}

-- | Decode JSON string to 'Data.String'
string :: Decoder String
string = auto
{-# INLINE string #-}

-- | Decode JSON string to 'Data.Text'
text :: Decoder Text
text = auto
{-# INLINE text #-}

-- | Decode JSON string to 'Data.UUID.Types.UUID'
uuid :: Decoder UUID
uuid = auto
{-# INLINE uuid #-}

-- | Decode JSON string to 'Data.Version'
version :: Decoder Version
version = auto
{-# INLINE version #-}

-- | Decode JSON string to 'Data.Local.Time.ZonedTime'
-- using Aeson's instance implementation.
--
-- Supported string formats:
--
-- YYYY-MM-DD HH:MM Z YYYY-MM-DD HH:MM:SS Z YYYY-MM-DD HH:MM:SS.SSS Z
--
-- The first space may instead be a T, and the second space is optional. The Z represents UTC. The Z may be replaced with a time zone offset of the form +0000 or -08:00, where the first two digits are hours, the : is optional and the second two digits (also optional) are minutes.
zonedTime :: Decoder ZonedTime
zonedTime = auto
{-# INLINE zonedTime #-}

-- | Decode JSON string to 'Data.Local.Time.LocalTime'
-- using Aeson's instance implementation.
localTime :: Decoder LocalTime
localTime = auto
{-# INLINE localTime #-}

-- | Decode JSON string to 'Data.Local.Time.TimeOfDay'
-- using Aeson's instance implementation.
timeOfDay :: Decoder TimeOfDay
timeOfDay = auto
{-# INLINE timeOfDay #-}

-- | Decode JSON string to 'Data.Time.Clock.UTCTime'
-- using Aesons's instance implementation
utcTime :: Decoder UTCTime
utcTime = auto
{-# INLINE utcTime #-}

-- | Decode JSON string to 'Data.Time.Calendar.Day'
-- using Aesons's instance implementation
day :: Decoder Day
day = auto
{-# INLINE day #-}

-- | Decode JSON string to 'Data.Time.Calendar.Compat.DayOfWeek'
-- using Aesons's instance implementation
dayOfWeek :: Decoder DayOfWeek
dayOfWeek = auto
{-# INLINE dayOfWeek #-}


-- Continer Decoders

-- | Decode JSON null and other JSON value to 'Data.Maybe'.
-- JSON null will be decoded to 'Nothing'.
-- Other value decoded by provided 'Decoder' to 'Just'
nullable :: Decoder a -> Decoder (Maybe a)
nullable (Decoder d) = Decoder $ \case
  Null  -> pure Nothing
  other -> Just <$> d other
{-# INLINE nullable #-}

-- | Decode JSON array of values to '[a]' of values
-- using provided 'Decoder'.
list :: Decoder a -> Decoder [a]
list (Decoder d) = Decoder $
  listParser d
{-# INLINE list #-}

-- | Decode JSON array of values to 'Vector' of values
-- using provided 'Decoder'.
vector :: Decoder a -> Decoder (Vector a)
vector (Decoder d) = Decoder $ \case
  Array v -> Vector.mapM d v
  other   -> typeMismatch "Array" other
{-# INLINE vector #-}

-- | Decode JSON object to 'HL.HashMap' with 'Data.Text' key
-- using provided 'Decoder'.
hashMapLazy :: Decoder a -> Decoder (HL.HashMap Text a)
hashMapLazy (Decoder d) = Decoder $ \case
  Object xs -> traverse d xs
  val -> typeMismatch "Array" val
{-|# INLINE hashMapLazy #-}

-- | Decode JSON object to 'HS.HashMap' with 'Data.Text' key
-- using provided 'Decoder'.
hashMapStrict :: Decoder a -> Decoder (HS.HashMap Text a)
hashMapStrict (Decoder d) = Decoder $ \case
  Object xs -> traverse d xs
  val -> typeMismatch "Array" val
{-|# INLINE hashMapStrict #-}

-- | Decode JSON object to 'ML.Map' with 'Data.Text' key
-- using provided 'Decoder'.
mapLazy :: Decoder a -> Decoder (ML.Map Text a)
mapLazy dec = ML.fromList . HL.toList <$> hashMapLazy dec
{-|# INLINE mapStrict #-}

-- | Decode JSON object to 'MS.Map' with 'Data.Text' key
-- using provided 'Decoder'.
mapStrict :: Decoder a -> Decoder (MS.Map Text a)
mapStrict dec = MS.fromList . HL.toList <$> hashMapLazy dec
{-|# INLINE mapStrict #-}


-- Combinators

-- | Decode JSON null to any value.
-- This function is usefull if you have custom
-- constructor which represented by null in JSONs.
--
-- > data Codomain = NotSet | Foo | Bar
-- >
-- > myDomainDecoder :: Decoder Codomain
-- > myDomainDecoder = jsonNull NotSet
-- >               <|> (text >>= fooBar)
-- >    where fooBar "foo"   = return Foo
-- >          fooBar "bar"   = return Bar
-- >          fooBar unknown = fail $ "Unknown value " <> show unknown
jsonNull :: a -> Decoder a
jsonNull a = Decoder $ \case
  Null -> pure a
  val    -> typeMismatch "null" val
{-# INLINE jsonNull #-}

-- | Extract JSON value from JSON object key
--
-- > >>> decode (key "data" int) "{\"data\": 42}"
-- > Just 42
key :: Text -> Decoder a -> Decoder a
key t (Decoder d) = Decoder $ \case
  Object v -> d =<< v .: t
  val        -> typeMismatch "Object" val
{-# INLINE key #-}

-- | Extract JSON value from JSON object keys
--
-- > >>> decode (at ["data", "value"] int) "{\"data\": {\"value\": 42}}"
-- > Just 42
at :: [Text] -> Decoder a -> Decoder a
at pth d = foldr key d pth
{-# INLINE at #-}

-- | Extract JSON value from JSON array index
--
-- > >>> decode (index 2 int) "[0,1,2,3,4]"
-- > Just 2
index :: Int -> Decoder a -> Decoder a
index i (Decoder d) = Decoder $ \val ->
  case val of
    Array vec -> case vec !? i of
                    Just v  -> d v
                    Nothing -> unexpected val
    _         -> typeMismatch "Array" val
{-# INLINE index #-}

-- | Extract JSON value from JSON array indexes
--
-- > >>> decode (indexes [0,1,0] int) "[[true, [42]]]"
-- > Just 42
indexes :: [Int] -> Decoder a -> Decoder a
indexes pth d = foldr index d pth
{-# INLINE indexes #-}

-- $jsonpath
-- Combinators using Aeson's 'JSONPathElement' and 'JSONPath' types.
-- This makes it possible to mix object keys and array index accessors.

-- | Decode value from JSON structure.
--
-- From object key:
--
-- > >>> decode (element (Key "data") text) "{\"data\": \"foo\"}"
-- > Just "foo"
--
-- From array index:
--
-- > >>> decode (element (Index 1) int) "[0,1,2]"
-- > Just 1
element :: JSONPathElement -> Decoder a -> Decoder a
element (Key txt) = key txt
element (Index i) = index i
{-# INLINE element #-}

-- | Decode value from deep JSON structure.
--
-- > >>> decode (path [Key "data", Index 0] bool) "{\"data\":[true, false, false]}"
-- > Just True
path :: JSONPath -> Decoder a -> Decoder a
path pth d = foldr element d pth
{-# INLINE path #-}


-- Decoding

-- $running
--
-- Following functions are evivalent to the one provided by Aeson.
-- The only difference is versions provided by Aeson
-- for with 'FromJSON' instances while these use 'Decoder' type
-- instead.

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses immediately, but defers conversion. See
-- 'Data.Aeson.json' for details.
decode :: Decoder a -> LB.ByteString -> Maybe a
decode (Decoder d) =
  Parser.decodeWith ParserI.jsonEOF (parse d)
{-# INLINE decode #-}

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses and performs conversion immediately. See
-- 'Data.Aeson.json'' for details.
decode' :: Decoder a -> LB.ByteString -> Maybe a
decode' (Decoder d) =
  Parser.decodeWith ParserI.jsonEOF' (parse d)
{-# INLINE decode' #-}

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: Decoder a -> LB.ByteString -> Either String a
eitherDecode (Decoder d) =
  eitherFormatError . Parser.eitherDecodeWith ParserI.jsonEOF (AI.iparse d)
{-# INLINE eitherDecode #-}

-- | Like 'decode'' but returns an error message when decoding fails.
eitherDecode' :: Decoder a -> LB.ByteString -> Either String a
eitherDecode' (Decoder d) =
  eitherFormatError . Parser.eitherDecodeWith ParserI.jsonEOF' (AI.iparse d)
{-# INLINE eitherDecode' #-}


-- Strict Decoding

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses immediately, but defers conversion. See
-- 'Data.Aeson.json' for details.
decodeStrict :: Decoder a -> B.ByteString -> Maybe a
decodeStrict (Decoder d) =
  Parser.decodeStrictWith ParserI.jsonEOF (parse d)
{-# INLINE decodeStrict #-}

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input must consist solely of a JSON document, with no trailing
-- data except for whitespace.
--
-- This function parses and performs conversion immediately.  See
-- 'Data.Aeson.json'' for details.
decodeStrict' :: Decoder a -> B.ByteString -> Maybe a
decodeStrict' (Decoder d) =
  Parser.decodeStrictWith ParserI.jsonEOF' (parse d)
{-# INLINE decodeStrict' #-}

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecodeStrict :: Decoder a -> B.ByteString -> Either String a
eitherDecodeStrict (Decoder d) =
  eitherFormatError . Parser.eitherDecodeStrictWith ParserI.jsonEOF (AI.iparse d)
{-# INLINE eitherDecodeStrict #-}

-- | Like 'decodeStrict'' but returns an error message when decoding fails.
eitherDecodeStrict' :: Decoder a -> B.ByteString -> Either String a
eitherDecodeStrict' (Decoder d) =
  eitherFormatError . Parser.eitherDecodeStrictWith ParserI.jsonEOF' (AI.iparse d)
{-# INLINE eitherDecodeStrict' #-}


-- File Decoding

-- | Efficiently deserialize a JSON value from a file.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input file's content must consist solely of a JSON document,
-- with no trailing data except for whitespace.
--
-- This function parses immediately, but defers conversion. See
-- 'Data.Aeson.json' for details.
decodeFileStrict :: Decoder a -> FilePath -> IO (Maybe a)
decodeFileStrict dec =
  fmap (decodeStrict dec) . B.readFile
{-# INLINE decodeFileStrict #-}

-- | Efficiently deserialize a JSON value from a file.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input file's content must consist solely of a JSON document,
-- with no trailing data except for whitespace.
--
-- This function parses and performs conversion immediately.  See
-- 'Data.Aeson.json'' for details.
decodeFileStrict' :: Decoder a -> FilePath -> IO (Maybe a)
decodeFileStrict' dec =
  fmap (decodeStrict' dec) . B.readFile
{-# INLINE decodeFileStrict' #-}

-- | Like 'decodeFileStrict' but returns an error message when decoding fails.
eitherDecodeFileStrict :: Decoder a -> FilePath -> IO (Either String a)
eitherDecodeFileStrict dec =
  fmap (eitherDecodeStrict dec) . B.readFile
{-# INLINE eitherDecodeFileStrict #-}

-- | Like 'decodeFileStrict'' but returns an error message when decoding fails.
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
