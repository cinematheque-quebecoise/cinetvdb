let
  pkgs = import ./nixpkgs.nix;
  project = import ./release.nix;
  haskellPackages = pkgs.haskell.packages.ghc883;

in pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.cabal2nix
    haskellPackages.hlint
    haskellPackages.stylish-haskell
    haskellPackages.brittany
  ];

  LANG = "en_US.UTF-8";

  shellHook = ''
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
  '';
}
