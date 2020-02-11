let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages =
        pkgs.haskellPackages.override {
            overrides = self: super: {
              dsjd = self.callCabal2nix "dsjd" ./. {};
            };
        };
    };
  };
  pkgs =
    import <nixpkgs> { inherit config; };
in pkgs.haskellPackages.dsjd
