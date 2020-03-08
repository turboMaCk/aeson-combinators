with (import ./default.nix);
{
  ghc = ghc.env;
  ghcjs = ghcjs.env;
}
