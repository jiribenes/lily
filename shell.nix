let
  release = import ./release.nix { };
in
  release.lily.env
