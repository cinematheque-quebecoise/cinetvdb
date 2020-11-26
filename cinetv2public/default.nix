{ mkDerivation, base, cinetv4h, directory, docopt, esqueleto
, filepath, hpack, persistent-sqlite, regex, resource-pool, stdenv
, text, unliftio
}:
mkDerivation {
  pname = "cinetv2public";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base cinetv4h directory docopt esqueleto filepath persistent-sqlite
    regex resource-pool text unliftio
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/cinetv2public#readme";
  # license = stdenv.lib.licenses.gpl3;
}
