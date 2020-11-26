import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  # name = "nixos-stable-20.03";
  # url = "https://github.com/nixos/nixpkgs-channels/";
  # ref = "refs/heads/nixos-20.03";
  # rev = "20904118004113e400633f080e73f51ac6080e14";

  name = "nixos-unstable-2020-04-29";
  url = "https://github.com/nixos/nixpkgs-channels/";
  ref = "refs/heads/nixos-unstable";
  rev = "7c399a4ee080f33cc500a3fda33af6fccfd617bd";
}) {}

