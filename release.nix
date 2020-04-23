{ compiler ? "ghc883" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              lily =
                haskellPackagesNew.callPackage ./lily.nix { };
              clang-pure =
                pkgs.haskell.lib.dontCheck (haskellPackagesNew.callPackage ./deps/clang-pure/clang-pure.nix { });
              inline-c =
                haskellPackagesNew.callPackage ./inline-c.nix { };
            };
          };
        };
      };
    };
  };  

  pkgs = import (import ./pinned-nixpkgs.nix) { inherit config; };
in 
  { lily = pkgs.haskell.packages.${compiler}.lily; }

