let
  pkgs = import ./nixpkgs.nix;

  haskellPackages = pkgs.haskell.packages.ghc883;

  docoptPkgs = haskellPackages.callCabal2nix "docopt" (builtins.fetchGit {
    url = "https://github.com/gelisam/docopt.hs.git";
    ref = "refs/heads/monad-fail";
    rev = "16dc7bc596c0ea4fa4466b12f474b1abfa72c885";
  }) {};
  callCabal2nix = haskellPackages.callCabal2nix;

  mkDerivation = pkgs.stdenv.mkDerivation;
in
  haskellPackages.callCabal2nix "cinetv2public" (./.) {
    docopt = pkgs.haskell.lib.dontCheck docoptPkgs;
    cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetv4h) {};
  }
