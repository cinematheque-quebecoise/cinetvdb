{ pkgs, cinetv4h }:
let
  inherit (pkgs) haskellPackages;

  # rdf4hPackage = haskellPackages.callCabal2nix "rdf4h" (builtins.fetchGit {
  #   url = "https://github.com/robstewart57/rdf4h.git";
  #   ref = "refs/heads/master";
  #   rev = "35d7a7948fd869faa7eb98c7fe1c47c3bd341c24";
  # }) {};

  hsparqlPackage = haskellPackagesOverride.callCabal2nix "hsparql" (builtins.fetchGit {
    url = "https://github.com/robstewart57/hsparql.git";
    ref = "refs/heads/master";
    rev = "32d25fb1324663adf3241f42b61fcb8ce918c557";
  }) {};

  haskellPackagesOverride = haskellPackages.override {
    overrides = self: upser: rec {
      hsparql = hsparqlPackage;
      # rdf4h = pkgs.haskell.lib.dontCheck rdf4hPackage;
      # monoidal-containers = pkgs.haskell.lib.doJailbreak haskellPackages.monoidal-containers;
    };
  };

in
  haskellPackagesOverride.callCabal2nix "cinetvlinking" (./.) {
    inherit cinetv4h;
    # cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetv4h) {};
    # hsparql = hsparqlPackage;
  }
