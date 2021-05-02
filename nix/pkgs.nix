let
  # Pinned NixOS/nixpkgs 2021/04/26
  sources = import ./sources.nix {};
  pkgs = import sources.nixpkgs {};

  cinetv4h = import ../cinetv4h { inherit pkgs; };
  cinetv2public = import ../cinetv2public { inherit pkgs cinetv4h; };
  cinetvlinking = import ../cinetvlinking { inherit pkgs cinetv4h; };

  sqlite-dump-to-csv = import ./sqlite-dump-to-csv.nix { inherit pkgs; };

  haskellPackages = pkgs.haskell.packages.ghc8104.override {
    overrides = hself: hsuper: {
      inherit cinetv4h;
      inherit cinetv2public;
      inherit cinetvlinking;
    };
  };

  drv = pkgs.stdenv.mkDerivation rec {
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
      cp ${../cinetv2sqlite.py} $out/bin/cinetv2sqlite
      chmod +x $out/bin/cinetv2sqlite
      wrapProgram $out/bin/cinetv2sqlite \
        --prefix PATH : "${pkgs.python38Packages.csvs-to-sqlite}/bin:${cinetv2public}/bin:${cinetvlinking}/bin:${sqlite-dump-to-csv}/bin"
    '';
  };

  shell = haskellPackages.shellFor {
    packages = p: with p; [
      p.cinetv4h
      p.cinetv2public
      p.cinetvlinking
    ];

    buildInputs = with pkgs; [
      cabal-install
      python38
      jq
      curl
      cacert
      gzip
      git
      icu

      hlint
      ormolu
      haskell-language-server

      (pkgs.python38.withPackages (pythonPackages: with pythonPackages; [
        csvs-to-sqlite
      ]))
    ];
  };
in
  {
    inherit shell;
    inherit drv;
  }
