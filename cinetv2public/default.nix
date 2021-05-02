{ pkgs, cinetv4h }:

let
  inherit (pkgs) haskellPackages;

  docoptPkgs = haskellPackages.callCabal2nix "docopt" (builtins.fetchGit {
    url = "https://github.com/gelisam/docopt.hs.git";
    ref = "refs/heads/monad-fail";
    rev = "16dc7bc596c0ea4fa4466b12f474b1abfa72c885";
  }) {};

  mkDerivation = pkgs.stdenv.mkDerivation;
in
  haskellPackages.callCabal2nix "cinetv2public" (./.) {
    inherit cinetv4h;
    docopt = pkgs.haskell.lib.dontCheck docoptPkgs;
  }
