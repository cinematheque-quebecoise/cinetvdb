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

  inherit (pkgs) haskellPackages;

  # haskellPackages = pkgs.haskell.packages.${compiler};
  # haskellPackages.callCabal2nix "cinetv2public" (./.) {
  #   cinetv4h = haskellPackages.callCabal2nix "cinetv4h" (./../cinetv4h) {};
  # }
  # callCabal2nix = pkgs.haskell.packages.ghc883.callCabal2nix;

  project = import ./release.nix;
in
pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.hpack
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.brittany

    pkgs.zlib
  ];

  LANG = "en_US.UTF-8";

  shellHook = ''
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
  '';
}
