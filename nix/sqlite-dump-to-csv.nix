{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "sqlite-dump-to-csv";
  version = "0.1";

  src = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/ekansa/sqlite-dump-to-csv/master/sqlite_dump.py";
    sha256 = "0fss4dclhsf8qyk3hfcihw6dnn3llhvdw77vh6s6rfm86x2klaa2";
  };

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out/bin
    cp ${src} $out/bin/sqlite-dump-to-csv
    chmod +x $out/bin/sqlite-dump-to-csv
  '';
}
