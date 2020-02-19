<div align="center">
    <h1>Aeson Combinators</h1>
    <p>Aeson combinators for dead simple JSON decoding</p>
</div>

Aeson decoding API is closed over the type class `FromJSON`.
Because of this there is one to one mapping between JSON
format and data decoded from it.
While this is handy in many situations it forces
users of Aeson library to define proxy types and
data wrappers just for sake of implementing instance
of `FromJSON` and `ToJSON`.

This library defines [**low overhead**](#internals) value space decoder
and that eliminate this such type bloating.

This library is not replacement for Aeson's type classes but
rather value level extensions of them. [**Combinators and
instances can be mixed and match**](#complimentary-usage-with-type-classes). Optionally it
can be used to replace `FromJSON` instances "completely"
and be used for [**elm like decoding**](#elm-style-decoding)
or to make Aeson feel more like [**waargonaut**](https://hackage.haskell.org/package/waargonaut).

## Status

**This package is in pre-alpha stage and is not published to Hackage yet**.
The whole idea is in an early stage. I might be easily wrong that this is a good idea
and eventually abandon this project. If you have any feedback feel free to contact me:

- twitter [@turbo_MaCk](https://twitter.com/turbo_MaCk)
- email marek.faj@gmail.com

## Complimentary Usage with Type Classes

As mentioned above, combinators and type classes can be mixed and match.


**Decode type nested in json:**

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Text
import Data.ByteString.Lazy (ByteString)
import Data.Aeson.Types
import qualified Data.Aeson.Combinators.Decode as ACD
import GHC.Generics

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)

instance FromJSON Person

decodeEmbededPerson :: ByteString -> [Text] -> Maybe Person
decodeEmbededPerson json path =
    ACD.decode (ACD.at path ACD.auto) json
```

Now we can extract Person from any key within the json:

```haskell
>>> decodeEmbededPerson "{\"data\": {\"person\":{\"name\":\"Joe\",\"age\":12}}}" ["data", "person"]
Just (Person {name = "Joe", age = 12})
```

**Easily decode multiple data from single json:**

```haskell
-- data Person defined above ^

type Token = Text

decodePersonWithToken :: ByteString -> Maybe (Token, Person)
decodePersonWithToken json =
    ACD.decode decoder json
    where decoder =
            (,) <$> ACD.field "token" ACD.auto
                <*> ACD.field "person" ACD.auto
```

Which can be used:

```haskell
>>> decodePersonWithToken "{\"person\":{\"name\":\"Joe\",\"age\":12}, \"token\": \"foo\"}"
Just ("foo",Person {name = "Joe", age = 12})
```

## Elm Style Decoding

If you like elm style decoding you can avoid using FromJSON type class all togher:

```haskell
import Data.Text
import Data.Aeson.Combinators.Decode

data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Show)

personDecoder :: Decoder Person
personDecoder =
    Person
        <$> field "name" auto
        <*> field "age" auto
```

and use it directly as:

```haskell
>>> decode personDecoder "{\"name\":\"Joe\",\"age\":12}"
Just (Person {name = "Joe", age = 12})
```

## Internals

This library introduces as low overhead over Aeson API as possible.
`Decoder a` type is a function `Value -> Parser a` same as `fromJSON`
function of `FromJSON` class. This means there should be near zero overhead.
Aeson types and functions are reused where possible.

## TODO

- [ ] Decoding of Containers and Vector Types with Decoder
- [ ] Encoding API
- [ ] Documentation
- [ ] Support for more GHC versions

## License

(c) 2020 Marek Fajkus
BSD-3-Clause
