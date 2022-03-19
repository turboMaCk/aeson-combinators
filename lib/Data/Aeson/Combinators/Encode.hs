{-# LANGUAGE CPP #-}

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

-- * Aleternative to using 'Encode' Combinators
-- $alternative

-- * Example Usage
-- $usage

-- * Encoder
    Encoder(..)
  , auto
  , run
-- * Object Encoding
-- $objects
  , KeyValueEncoder
  , object
  , field
-- ** Alternative Object Encoding
  , KeyValueEncoder'
  , object'
  , field'
-- * Collections
  , list
  , vector
  , jsonArray
-- * Encoding Primitive Values
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
-- * Encoding Time
  , zonedTime, localTime, timeOfDay
  , utcTime
  , day
#if (MIN_VERSION_time_compat(1,9,2))
  , dayOfWeek
#endif
  -- * Evaluating Encoders
  , encode
  , toEncoding
  -- * Re-expose aeson internals we depend on
  , module Data.Aeson.Combinators.Compat
) where

import           Control.Applicative
import           Control.Monad              (join)
import           Data.Functor.Contravariant

import           Data.Aeson                 (ToJSON, Value (..))
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Combinators.Compat
import qualified Data.Aeson.Encoding        as E
import           Data.Aeson.Types           (Pair)
import qualified Data.ByteString.Lazy       as BS
import           Data.Text                  (Text)
import           Data.Vector                (Vector, fromList, (!?))
import qualified Data.Vector                as Vector

import           Data.Int                   (Int16, Int32, Int64, Int8)
import           Data.Time.Calendar         (Day)
#if (MIN_VERSION_time_compat(1,9,2))
import           Data.Time.Calendar.Compat  (DayOfWeek)
#endif
import           Data.Time.Clock            (UTCTime)
import           Data.Time.LocalTime        (LocalTime, TimeOfDay, ZonedTime)
import           Data.UUID.Types            (UUID)
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

And first encoder for this type:

>>> :{
personEncoder :: Encoder Person
personEncoder = object
  [ field "name" string name
  , field "age" int age
  ]
:}

We can use this 'Encoder' to encode value into JSON:

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

-}


{-|
Value describing encoding of @a@ into a JSON 'Value'.
This is essentially just a wrapper around function that
should be applied later.

=== Covariant to map function over input

Given:

>>> :{
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq)
:}

>>> :{
personEncoder :: Encoder Person
personEncoder = object
  [ field "name" string name
  , field "age" int age
  ]
:}

We can extract person from any pair:

>>> :{
-- Using personEncoder definition from example above
pairEncoder2 :: Encoder (Person, a)
pairEncoder2 = contramap fst personEncoder
:}

>>> encode pairEncoder2 (Person "Jane" 42, Nothing)
"{\"age\":42,\"name\":\"Jane\"}"

=== Divisible and Decidable

Some of you might know library @covariant@ and ask what is a support for
other covariant typeclasses.
It's not possible to define lawful Divisble instance for JSON 'Value'
and by extension it's not possible to define Decidable either.
While it is posible to provide somewhat useful unlawful instances for these this
library opts to not to do that.

-}
newtype Encoder a = Encoder (a -> Value)


instance Contravariant Encoder where
  contramap f (Encoder enc) = Encoder (enc . f)
  {-# INLINE contramap #-}


-- | Run 'Encoder' given a value. this is essentially just a function application.
run :: Encoder a -> a -> Value
run (Encoder f) = f
{-# INLINE run #-}


-- | "Grab" 'Encoder' from 'ToJSON' definition.
auto :: ToJSON a => Encoder a
auto = Encoder Aeson.toJSON
{-# INLINE auto #-}


-- Combinators

{- $objects
There are two alternative ways of defining Object encodings.
Both provide "eqvivalent" types and functions with consistent naming.
Variants without and with @'@ suffix are meant to be used together.
-}

{-| Object Encoder

>>> :{
  data Object = Object
    { name :: Text
    , age  :: Int
    } deriving (Show, Eq)
:}

>>> :{
  objectEncoder :: Encoder Object
  objectEncoder = object
    [ field "name" text name
    , field "age" int age
    ]
:}

>>> encode objectEncoder $ Object "Joe" 30
"{\"age\":30,\"name\":\"Joe\"}"

-}
type KeyValueEncoder a = a -> Pair


{-| Object combinators -}
object :: [KeyValueEncoder a] -> Encoder a
object xs = Encoder $ \val -> Aeson.object $ fmap (\f -> f val) xs
{-# INLINE object #-}


{-| Define object field -}
field :: Key -> Encoder b -> (a -> b) -> KeyValueEncoder a
field name (Encoder enc) get v = (name, enc $ get v)
{-# INLINE field #-}


{-| Object Encoder (alternative)

>>> :set -XRecordWildCards

>>> :{
  data Object = Object
    { name :: Text
    , age  :: Int
    } deriving (Show, Eq)
:}

>>> :{
  objectEncoder' :: Encoder Object
  objectEncoder' = object' $ \Object{..} ->
    [ field' "name" text name
    , field' "age" int age
    ]
:}

>>> encode objectEncoder' $ Object "Joe" 30
"{\"age\":30,\"name\":\"Joe\"}"
-}
type KeyValueEncoder' a = a -> [Pair]


{-| Object combinators (alternative) -}
object' :: KeyValueEncoder' a -> Encoder a
object' f = Encoder $ \val -> Aeson.object $ f val
{-# INLINE object' #-}


{-| Define object field (alternative) -}
field' :: Key -> Encoder a -> a -> (Key, Value)
field' name (Encoder enc) val = (name, enc val)
{-# INLINE field' #-}


-- Collections


{-| Encode 'Vector' -}
vector :: Encoder a -> Encoder (Vector a)
vector (Encoder f) = Encoder $ \val -> Aeson.Array $ f <$> val
{-# INLINE vector #-}


{-| Encode 'List' -}
list :: Encoder a -> Encoder [a]
list (Encoder f) = Encoder $ \val -> Aeson.Array $ fromList $ f <$> val
{-# INLINE list #-}


{-| Encode multiple values as array -}
jsonArray :: [Encoder a] -> Encoder a
jsonArray xs = Encoder $ \a -> Array $ Vector.fromList $ (\(Encoder f) -> f a) <$> xs
{-# INLINE jsonArray #-}


-- Basic Encoders


-- | Encode any JSON value to 'Void' value
-- which is impossible to construct.
--
-- __This Encoder is guarenteed to fail.__
void :: Encoder Void
void = auto
{-# INLINE void #-}


-- | Encode JSON null into '()'
unit :: Encoder ()
unit = auto
{-# INLINE unit #-}


-- | Encode JSON booleans to Haskell 'Data.Bool'
bool :: Encoder Bool
bool = auto
{-# INLINE bool #-}


-- | Encode JSON number to 'Data.Int.Int'
int :: Encoder Int
int = auto
{-# INLINE int #-}


-- | Encode JSON number to 'Data.Int.Int8'
int8 :: Encoder Int8
int8 = auto
{-# INLINE int8 #-}


-- | Encode JSON number to 'Data.Int.Int16'
int16 :: Encoder Int16
int16 = auto
{-# INLINE int16 #-}


-- | Encode JSON number to 'Data.Int.Int32'
int32 :: Encoder Int32
int32 = auto
{-# INLINE int32 #-}


-- | Encode JSON number to 'Data.Int.Int64'
int64 :: Encoder Int64
int64 = auto
{-# INLINE int64 #-}


-- | Encode JSON number to unbounded 'Integer'
integer :: Encoder Integer
integer = auto
{-# INLINE integer #-}


#if (MIN_VERSION_base(4,8,0))
-- | Encode JSON number to GHC's 'GHC.Natural' (non negative)
--
-- This function requires 'base' >= 4.8.0
natural :: Encoder Natural
natural = auto
{-# INLINE natural #-}
#endif


-- | Encode JSON number to bounded 'Data.Word.Word'
word :: Encoder Word
word = auto
{-# INLINE word #-}


-- | Encode JSON number to bounded 'Data.Word.Word8'
word8 :: Encoder Word8
word8 = auto
{-# INLINE word8 #-}


-- | Encode JSON number to bounded 'Data.Word.Word16'
word16 :: Encoder Word16
word16 = auto
{-# INLINE word16 #-}


-- | Encode JSON number to bounded 'Data.Word.Word32'
word32 :: Encoder Word32
word32 = auto
{-# INLINE word32 #-}


-- | Encode JSON number to bounded 'Data.Word.Word64'
word64 :: Encoder Word64
word64 = auto
{-# INLINE word64 #-}


-- | Encode JSON number to 'Float'
float :: Encoder Float
float = auto
{-# INLINE float #-}


-- | Encode JSON number to 'Double'
double :: Encoder Double
double = auto
{-# INLINE double #-}


-- | Encode JSON number to arbitrary precision 'Scientific'
scientific :: Encoder Scientific
scientific = auto
{-# INLINE scientific #-}


-- | Encode single character JSON string to 'Data.Char'
char :: Encoder Char
char = auto
{-# INLINE char #-}


-- | Encode JSON string to 'Data.String'
string :: Encoder String
string = auto
{-# INLINE string #-}


-- | Encode JSON string to 'Data.Text'
text :: Encoder Text
text = auto
{-# INLINE text #-}


-- | Encode JSON string to 'Data.UUID.Types.UUID'
uuid :: Encoder UUID
uuid = auto
{-# INLINE uuid #-}


-- | Encode JSON string to 'Data.Version'
version :: Encoder Version
version = auto
{-# INLINE version #-}


-- | Encode JSON string to 'Data.Local.Time.ZonedTime'
-- using Aeson's instance implementation.
--
-- Supported string formats:
--
-- YYYY-MM-DD HH:MM Z YYYY-MM-DD HH:MM:SS Z YYYY-MM-DD HH:MM:SS.SSS Z
--
-- The first space may instead be a T, and the second space is optional. The Z represents UTC. The Z may be replaced with a time zone offset of the form +0000 or -08:00, where the first two digits are hours, the : is optional and the second two digits (also optional) are minutes.
zonedTime :: Encoder ZonedTime
zonedTime = auto
{-# INLINE zonedTime #-}


-- | Encode JSON string to 'Data.Local.Time.LocalTime'
-- using Aeson's instance implementation.
localTime :: Encoder LocalTime
localTime = auto
{-# INLINE localTime #-}


-- | Encode JSON string to 'Data.Local.Time.TimeOfDay'
-- using Aeson's instance implementation.
timeOfDay :: Encoder TimeOfDay
timeOfDay = auto
{-# INLINE timeOfDay #-}


-- | Encode JSON string to 'Data.Time.Clock.UTCTime'
-- using Aesons's instance implementation
utcTime :: Encoder UTCTime
utcTime = auto
{-# INLINE utcTime #-}


-- | Encode JSON string to 'Data.Time.Calendar.Day'
-- using Aesons's instance implementation
day :: Encoder Day
day = auto
{-# INLINE day #-}


#if (MIN_VERSION_time_compat(1,9,2))
-- | Encode JSON string to 'Data.Time.Calendar.Compat.DayOfWeek'
-- using Aesons's instance implementation
--
-- This function requires 'time-compat' >= 1.9.2
dayOfWeek :: Encoder DayOfWeek
dayOfWeek = auto
{-# INLINE dayOfWeek #-}
#endif


-- Encode


{-| Encode value into (Lazy) @ByteString@
-}
encode :: Encoder a -> a -> BS.ByteString
encode encoder =
  E.encodingToLazyByteString . toEncoding encoder
{-# INLINE encode #-}


{-| Convert value to encoding
-}
toEncoding :: Encoder a -> a -> E.Encoding
toEncoding (Encoder enc) = E.value . enc
{-# INLINE toEncoding #-}
