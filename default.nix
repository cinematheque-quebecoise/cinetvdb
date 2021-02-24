{ pkgs, ... }:

let
  pname = "cinetv2sqlite";
  cinetv2public = import ./cinetv2public/release.nix;
  # cinetvlinking = import ./cinetvlinking { inherit pkgs; };
  cinetvlinking = import ./cinetvlinking/release.nix;

  sqlite-dump-to-csv = import ./sqlite-dump-to-csv.nix { inherit pkgs; };
in
  pkgs.stdenv.mkDerivation rec {
    pname    = "cinetv2sqlite";
    version = "0.1.0";

    buildInputs = [
      (pkgs.python38.withPackages (pythonPackages: with pythonPackages; [
        docopt
        xlsx2csv
        csvs-to-sqlite
      ]))
    ];

    nativeBuildInputs = [ pkgs.makeWrapper ];

    unpackPhase = ":";
    installPhase = ''
      mkdir -p $out/bin
      cp ${./cinetv2sqlite.py} $out/bin/cinetv2sqlite
      chmod +x $out/bin/cinetv2sqlite
      wrapProgram $out/bin/cinetv2sqlite \
        --prefix PATH : "${pkgs.python38Packages.csvs-to-sqlite}/bin:${cinetv2public}/bin:${cinetvlinking}/bin:${sqlite-dump-to-csv}/bin"
    '';
  }
