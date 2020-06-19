{ pkgs ? import <nixpkgs> {} }:

let
  project = import ./release.nix;
in
pkgs.mkShell {
  name = "shell";
  buildInputs = [
    pkgs.python38
    pkgs.jq
    pkgs.curl
    pkgs.gzip
    pkgs.nix
    pkgs.git
    project
  ];

  shellHook = ''
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
  '';
}
