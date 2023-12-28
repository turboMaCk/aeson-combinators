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
  ghc8107 = haskell.packages.ghc8107.aeson-combinators;
  ghc902 = haskell.packages.ghc902.aeson-combinators;
  ghc925 = haskell.packages.ghc925.aeson-combinators;
}
