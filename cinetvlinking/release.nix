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

  compiler = "ghc883";
in
  pkgs.callPackage (./.) { }
