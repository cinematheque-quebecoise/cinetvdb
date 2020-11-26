{ mkDerivation, base, bytestring, cassava, cinetv4h, containers
, directory, either, esqueleto, filepath, foldl, hpack, hsparql
, hspec, http-client, monoidal-containers, neat-interpolation
, optparse-simple, persistent, persistent-sqlite, random
, resource-pool, rio, sparql-protocol, stdenv, tabl, text, text-icu
, transformers, unliftio, unordered-containers, vector
}:
mkDerivation {
  pname = "cinetvlinking";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava cinetv4h containers directory either
    esqueleto filepath foldl hsparql http-client monoidal-containers
    neat-interpolation persistent persistent-sqlite random
    resource-pool rio sparql-protocol tabl text text-icu transformers
    unliftio unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cassava cinetv4h containers directory either
    esqueleto filepath foldl hsparql http-client monoidal-containers
    neat-interpolation optparse-simple persistent persistent-sqlite
    random resource-pool rio sparql-protocol tabl text text-icu
    transformers unliftio unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring cassava cinetv4h containers directory either
    esqueleto filepath foldl hsparql hspec http-client
    monoidal-containers neat-interpolation persistent persistent-sqlite
    random resource-pool rio sparql-protocol tabl text text-icu
    transformers unliftio unordered-containers vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/cinetvlinking#readme";
  license = stdenv.lib.licenses.bsd3;
}
