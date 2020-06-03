{ compiler ? "ghc883" }:
let
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  pkgs = import (import ./pinned-nixpkgs.nix) { inherit config; };
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              lily = haskellPackagesNew.callCabal2nix "lily" (gitIgnore ./.) { };
              clang-pure =
                pkgs.haskell.lib.dontCheck (haskellPackagesNew.callPackage ./deps/clang-pure/clang-pure.nix { });
              inline-c =
                haskellPackagesNew.callPackage ./deps/inline-c.nix { };
            };
          };
        };
      };
    };
  };
  myHaskellPackages = pkgs.haskell.packages.${compiler};

  # development nix-shell
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.lily
    ];

    buildInputs = with myHaskellPackages; [
      # add developer tools here:
      cabal-install
      cabal2nix

      # IDE-related
      ghcid
      # ghcide # TODO: allow this after 0.2.0 hits nixpkgs
      hlint

      # formatters
      brittany
      pkgs.nixpkgs-fmt

      # LLVM libs
      pkgs.llvmPackages_9.clang-unwrapped
      pkgs.llvmPackages_9.clang-unwrapped.lib
      pkgs.llvmPackages_9.llvm
    ];

    withHoogle = true;
  };

  # static executable
  static-exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.lily);

  # docker image
  docker = pkgs.dockerTools.buildImage {
    name = "lily-docker";
    tag = "latest";
    created = "now";
    extraCommands = ''
      mkdir ./data
      chmod 777 ./data
    '';
    config = {
      Cmd = [ "${static-exe}/bin/lily" ];
      Env = [ "PATH=${static-exe}/bin" ];
      WorkingDir = "/data";
    };
  };
in
{
  inherit shell; # equivalent to `shell = shell`
  inherit static-exe;
  inherit docker;

  lily = myHaskellPackages.lily;
}
