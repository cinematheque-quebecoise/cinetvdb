import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-stable-20.03";
  url = "https://github.com/nixos/nixpkgs-channels/";
  # Commit hash for nixos-unstable as of 2018-09-12
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/nixos-20.03";
  rev = "20904118004113e400633f080e73f51ac6080e14";
}) {}

