let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
            overrides = self: super: {
              aeson-combinators = pkgs.haskell.lib.doBenchmark (self.callCabal2nix "aeson-combinators" ./. {});
            };
        };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

  # Using miso's ghcjs
  # cachix use miso-haskell
  # see https://github.com/dmjio/miso/blob/e222a66566c0377738791ab563054bbfbe6abd15/README.md#nix
  ghcjs = import (builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/843bdc3.tar.gz";
    sha256 = "sha256:1fc15jza8i6xz9b32jphi3yb8mfbdb3nd9m1wmzr68saqizbfdc0";
  }) {};
in with pkgs; {
  ghc = haskellPackages.aeson-combinators;
  ghcjs = ghcjs.pkgs.haskell.packages.ghcjs.callCabal2nix "aeson-combinators" ./. {};
  ghc94 = haskell.packages.ghc94.aeson-combinators;
  ghc96 = haskell.packages.ghc96.aeson-combinators;
  ghc98 = haskell.packages.ghc98.aeson-combinators;
  ghc910 = haskell.packages.ghc910.aeson-combinators;
}
