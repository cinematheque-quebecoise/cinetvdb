{ pkgs }:
let
  inherit (pkgs) haskellPackages;
in
  haskellPackages.callCabal2nix "cinetv4h" (./.) {}
