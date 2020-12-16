let
  pkgs = import ./nixpkgs.nix;

  inherit (pkgs) haskellPackages;

  project = import ./release.nix;
in
pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.hpack
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.brittany

    pkgs.zlib
  ];

  LANG = "en_US.UTF-8";

  shellHook = ''
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
  '';
}
