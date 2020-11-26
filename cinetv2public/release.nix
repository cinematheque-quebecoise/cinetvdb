let
  pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-unstable-2020-04-29";
    url = "https://github.com/nixos/nixpkgs-channels/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
    ref = "refs/heads/nixos-unstable";
    rev = "7c399a4ee080f33cc500a3fda33af6fccfd617bd";
  }) {};
  # pkgs = import ./nixpkgs.nix;

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
  # pkgs.callPackage (./.) {
  #   inherit mkDerivation;
  #   # inherit (pkgs) stdenv;
  #   inherit (haskellPackages) base;
  #   cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetv4h) {};
  #   inherit (haskellPackages) text;
  #   inherit (haskellPackages) directory;
  #   docopt = pkgs.haskell.lib.dontCheck docoptPkgs;
  #   inherit (haskellPackages) esqueleto;
  #   inherit (haskellPackages) filepath;
  #   inherit (haskellPackages) hpack;
  #   inherit (haskellPackages) persistent-sqlite;
  #   inherit (haskellPackages) regex;
  #   inherit (haskellPackages) resource-pool;
  #   inherit (haskellPackages) unliftio;
  # }
