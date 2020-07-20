{ sources ? import ./nix/sources.nix, nixpkgs ? sources.nixpkgs, compiler ? "ghc883" }:
let
  pkgs = import nixpkgs { inherit config; };

  # use gitignore to scout which files are important and which aren't
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  # custom package overrides
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              # this package:
              lily = haskellPackagesNew.callCabal2nix "lily" (gitIgnore ./.) { };

              # override clang-pure by our fork:
              clang-pure =
                pkgs.haskell.lib.dontCheck (haskellPackagesNew.callPackage ./deps/clang-pure/clang-pure.nix { });

              # call a specific version of inline-c:  
              inline-c =
                haskellPackagesNew.callPackage ./deps/inline-c.nix { };
            };
          };
        };
      };
    };
  };

  # synonym for easier use:
  myHaskellPackages = pkgs.haskell.packages.${compiler};

  # development nix-shell
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.lily
    ];

    buildInputs = with myHaskellPackages; [
      # DEVELOPMENT TOOLS:
      ## build-related:
      cabal-install
      cabal2nix

      ## IDE-related:
      ghcid
      ghcide
      hlint

      ## formatters
      brittany
      pkgs.nixpkgs-fmt

      # Niv for pinning dependencies
      (import sources.niv { }).niv

      # LLVM libs
      pkgs.llvmPackages_9.clang-unwrapped
      pkgs.llvmPackages_9.clang-unwrapped.lib
      pkgs.llvmPackages_9.llvm
    ];

    # enable Hoogle (will be present in the shell)
    withHoogle = true;
  };

  # static-ish executable
  static-exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.lily);

  # Docker image for transitive closure of `static-exe`!
  docker = pkgs.dockerTools.buildImage {
    name = "lily-docker";

    tag = "latest";

    created = "now"; # this technically breaks binary reproducibility, but it's useful for our purposes 

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
