# Aeson Combinators

[![Build Status](https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2FturboMaCk%2Faeson-combinators%2Fbadge%3Fref%3Dmaster&style=flat)](https://actions-badge.atrox.dev/turboMaCk/aeson-combinators/goto?ref=master)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

[**Low overhead**](#performance) value space `Decoder`
on top of Aeson's Parser for combinator style decoding.

This library is compatible with GHC as well as recent versions of **GHCJS**.

## Performance

`Decoder a` type is a function `Value -> Parser a` the same as `parseJSON`
member function of `FromJSON` class. This means there should be near zero overhead.
Aeson types and functions are reused where possible. Similarly `Encoder a` type
follow `toJSON` from `ToJSON` type class.

Simple benchmark shows that implementation using aeson-combinators performs better
than equivalent derived instance and on par (actually even slightly better though with difference in noise range)
with manually implemented instance.

## License

(c) 2020 Marek Fajkus
BSD-3-Clause
