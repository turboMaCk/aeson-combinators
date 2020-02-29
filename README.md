# Aeson Combinators

[![Build Status](https://travis-ci.org/turboMaCk/aeson-combinators.svg?branch=master)](https://travis-ci.org/turboMaCk/aeson-combinators)

This library defines [**low overhead**](#internals) value space `Decoder`
on top of Aeson's Parser for combinator style decoding.

__Encoding to JSON is currently not supported but might be added in the future version.__

I wrote a [blob post](https://turbomack.github.io/posts/2020-02-21-value-space-decoding-for-aeson.html)
describing what this library attempts to solve.

## Internals

This library introduces as low overhead over Aeson API as possible.
`Decoder a` type is a function `Value -> Parser a` same as `fromJSON`
function of `FromJSON` class. This means there should be near zero overhead.
Aeson types and functions are reused where possible.

## TODO

- [x] Documentation
- [x] Support for more GHC versions
- [x] Default decoders for Containers
- [ ] Encoding API

## License

(c) 2020 Marek Fajkus
BSD-3-Clause
