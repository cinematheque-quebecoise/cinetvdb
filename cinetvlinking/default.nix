{ pkgs, compiler ? "ghc883" }:
let
  rdf4hPackage = pkgs.haskell.packages.${compiler}.callCabal2nix "rdf4h" (builtins.fetchGit {
    url = "https://github.com/robstewart57/rdf4h.git";
    ref = "refs/heads/master";
    rev = "9658c9c361fb175f614a127c96da8ec42665334a";
  }) {};

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: upser: rec {
      rdf4h = pkgs.haskell.lib.dontCheck rdf4hPackage;

      hsparql = self.callCabal2nix "rdf4h" (builtins.fetchGit {
        url = "https://github.com/robstewart57/hsparql.git";
        ref = "refs/heads/master";
        rev = "ac11fa787aa4317675d34ccb0009b7cda8b87550";
      }) {};
    };
  };
in
  haskellPackages.callCabal2nix "cinetvlinking" (./.) {
    cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetv4h) {};
    hsparql = pkgs.haskell.lib.dontCheck haskellPackages.hsparql;
  }
