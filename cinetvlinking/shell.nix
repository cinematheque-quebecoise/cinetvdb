let
  pkgs = import ./nixpkgs.nix;
  project = import ./release.nix;

in pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.haskellPackages.cabal-install
  ];

  LANG = "en_US.UTF-8";

  shellHook = ''
    export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
  '';
}
