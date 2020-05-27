{ pkgs ? import <nixpkgs> {} }:

let
  project = import ./release.nix;
in
pkgs.mkShell {
  name = "shell";
  buildInputs = [
    pkgs.python38
    project
 ];
}
