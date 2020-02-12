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
  pkgs =
    import <nixpkgs> { inherit config; };
in pkgs.haskellPackages.aeson-combinators
