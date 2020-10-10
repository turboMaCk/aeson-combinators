# Aeson Combinators

[![Build Status](https://travis-ci.org/turboMaCk/aeson-combinators.svg?branch=master)](https://travis-ci.org/turboMaCk/aeson-combinators)
[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2FturboMaCk%2Faeson-combinators%2Fbadge%3Fref%3Dmaster&style=flat)](https://actions-badge.atrox.dev/turboMaCk/aeson-combinators/goto?ref=master)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

This library defines [**low overhead**](#internals) value space `Decoder`
on top of Aeson's Parser for combinator style decoding.

This library is compatible with GHC **7.6** and later as well as recent versions of **GHCJS**.

__Encoding to JSON is currently not supported but might be added in the future version.__

I wrote a [blob post](https://turbomack.github.io/posts/2020-02-21-value-space-decoding-for-aeson.html)
describing what this library attempts to solve.

## Internals

`Decoder a` type is a function `Value -> Parser a` the same as `fromJSON`
member function of `FromJSON` class. This means there should be near zero overhead.
Aeson types and functions are reused where possible.

## TODO

- [x] Documentation
- [x] Support for more GHC versions
- [x] Default decoders for Containers
- [ ] Encoding API

## License

(c) 2020 Marek Fajkus
BSD-3-Clause
