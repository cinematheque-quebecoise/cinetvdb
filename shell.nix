let
  pkgs = import ./nixpkgs.nix;

  compiler = "ghc883";

  haskellPackages = pkgs.haskell.packages.${compiler};

  project = import ./release.nix;
in
pkgs.mkShell {
  name = "shell";
  buildInputs = [
    pkgs.python38
    pkgs.jq
    pkgs.curl
    pkgs.cacert
    pkgs.gzip
    pkgs.nix
    pkgs.git

    (pkgs.python38.withPackages (pythonPackages: with pythonPackages; [
      csvs-to-sqlite
    ]))

  ];
}
