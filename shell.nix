let
  release = import ./release.nix { };
in
release.shell
