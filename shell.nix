let
  release = import ./release.nix { };
  
  drv = release.lily;
in
  drv.env
