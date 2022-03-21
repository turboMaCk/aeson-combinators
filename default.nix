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
    url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
    sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
  }) {};
in with pkgs; {
  ghc = haskellPackages.aeson-combinators;
  ghcjs = ghcjs.pkgs.haskell.packages.ghcjs.callCabal2nix "aeson-combinators" ./. {};
  ghc865 = haskell.packages.ghc865.aeson-combinators;
  ghc883 = haskell.packages.ghc883.aeson-combinators;
  ghc884 = haskell.packages.ghc884.aeson-combinators;
  ghc8101 = haskell.packages.ghc8101.aeson-combinators;
  ghc921= haskell.packages.ghc921.aeson-combinators;
}
