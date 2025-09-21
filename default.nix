let
  pkgs = import <nixpkgs> {};

  build = compiler:
    (pkgs.haskell.packages."${compiler}".override {
      overrides = self: super: {
        aeson-combinators = pkgs.haskell.lib.doBenchmark (self.callCabal2nix "aeson-combinators" ./. {});
      };
    }).aeson-combinators;

  # Using miso's ghcjs
  # cachix use miso-haskell
  # see https://github.com/dmjio/miso/blob/e222a66566c0377738791ab563054bbfbe6abd15/README.md#nix
  ghcjs = import (builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/843bdc3.tar.gz";
    sha256 = "sha256:1fc15jza8i6xz9b32jphi3yb8mfbdb3nd9m1wmzr68saqizbfdc0";
  }) {};
in {
  ghc94 = build "ghc94";
  ghc96 = build "ghc96";
  ghc98 = build "ghc98";
  ghc910 = build "ghc910";
  ghcjs = ghcjs.pkgs.haskell.packages.ghcjs.callCabal2nix "aeson-combinators" ./. {};
}
