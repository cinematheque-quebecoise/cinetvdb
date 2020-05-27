{ pkgs, compiler ? "ghc883" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};
in
  haskellPackages.callCabal2nix "cinetv4h" (./.) {}
