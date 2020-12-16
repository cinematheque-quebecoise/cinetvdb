{ pkgs, ... }:

let
  pname = "cinetv2sqlite";
  cinetv2public = import ./cinetv2public/release.nix;
  # cinetvlinking = import ./cinetvlinking { inherit pkgs; };
  cinetvlinking = import ./cinetvlinking/release.nix;
in
  pkgs.stdenv.mkDerivation rec {
    name    = "cinetv2sqlite-${version}";
    version = "0.1.0";

    buildInputs = [
      (pkgs.python38.withPackages (pythonPackages: with pythonPackages; [
        docopt
        xlsx2csv
        csvs-to-sqlite
      ]))
      # pkgs.python38Packages.csvs-to-sqlite
    ];

    nativeBuildInputs = [ pkgs.makeWrapper ];

    unpackPhase = ":";
    installPhase = ''
      mkdir -p $out/bin
      cp ${./cinetv2sqlite.py} $out/bin/cinetv2sqlite
      chmod +x $out/bin/cinetv2sqlite
      wrapProgram $out/bin/cinetv2sqlite \
        --prefix PATH : "${pkgs.python38Packages.csvs-to-sqlite}/bin:${cinetv2public}/bin:${cinetvlinking}/bin"
    '';
  }
