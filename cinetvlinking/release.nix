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

  # rdf4hPackage = haskellPackages.callCabal2nix "rdf4h" (builtins.fetchGit {
  #   url = "https://github.com/robstewart57/rdf4h.git";
  #   ref = "refs/heads/master";
  #   rev = "35d7a7948fd869faa7eb98c7fe1c47c3bd341c24";
  # }) {};

  haskellPackagesOverride = haskellPackages.override {
    overrides = self: upser: rec {
      # rdf4h = pkgs.haskell.lib.dontCheck rdf4hPackage;
      # monoidal-containers = pkgs.haskell.lib.doJailbreak haskellPackages.monoidal-containers;
    };
  };

  # hsparqlPackage = haskellPackagesOverride.callCabal2nix "hsparql" (builtins.fetchGit {
  #   url = "https://github.com/robstewart57/hsparql.git";
  #   ref = "refs/heads/master";
  #   rev = "ac11fa787aa4317675d34ccb0009b7cda8b87550";
  # }) {};

in
  haskellPackagesOverride.callCabal2nix "cinetvlinking" (./.) {
    cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetv4h) {};
    # hsparql = pkgs.haskell.lib.dontCheck hsparqlPackage;
  }
