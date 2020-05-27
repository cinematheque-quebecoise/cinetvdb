{ pkgs, ... }:

let
  pname = "create-cinetv-release";
  cinetv-migration = import ./cinetv-migration { inherit pkgs; };
  cinetvlinking = import ./cinetvlinking { inherit pkgs; };
in
  pkgs.stdenv.mkDerivation rec {
    name    = "create-cinetv-release-${version}";
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
      cp ${./create-cinetv-release.py} $out/bin/create-cinetv-release
      chmod +x $out/bin/create-cinetv-release
      wrapProgram $out/bin/create-cinetv-release \
        --prefix PATH : "${pkgs.python38Packages.csvs-to-sqlite}/bin:${cinetv-migration}/bin:${cinetvlinking}/bin"
    '';
  }
