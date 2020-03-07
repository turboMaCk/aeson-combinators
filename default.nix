let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
            overrides = self: super: {
              aeson-combinators = self.callCabal2nix "aeson-combinators" ./. {};
            };
        };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };

  # Using miso's ghcjs cache
  ghcjs = import (builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
    sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
  }) {};
in {
  ghc = pkgs.haskellPackages.aeson-combinators;
  ghcjs = ghcjs.pkgs.haskell.packages.ghcjs.callCabal2nix "aeson-combinators" ./. {};
}
