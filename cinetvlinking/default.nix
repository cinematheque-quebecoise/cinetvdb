{ pkgs, compiler ? "ghc883" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};

  rdf4hPackage = haskellPackages.callCabal2nix "rdf4h" (builtins.fetchGit {
    url = "https://github.com/robstewart57/rdf4h.git";
    ref = "refs/heads/master";
    rev = "35d7a7948fd869faa7eb98c7fe1c47c3bd341c24";
  }) {};

  haskellPackagesOverride = haskellPackages.override {
    overrides = self: upser: rec {
      rdf4h = pkgs.haskell.lib.dontCheck rdf4hPackage;
    };
  };

  hsparqlPackage = haskellPackagesOverride.callCabal2nix "hsparql" (builtins.fetchGit {
    url = "https://github.com/robstewart57/hsparql.git";
    ref = "refs/heads/master";
    rev = "ac11fa787aa4317675d34ccb0009b7cda8b87550";
  }) {};

in
  haskellPackagesOverride.callCabal2nix "cinetvlinking" (./.) {
    cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetv4h) {};
    hsparql = pkgs.haskell.lib.dontCheck hsparqlPackage;
  }
