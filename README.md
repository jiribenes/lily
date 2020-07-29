# Lily

Lily is an experimental C++ linter
based on linear types.
It is a result of my bachelor thesis.

## Downloading the repository

```
git clone --recursive-submodules $REPO_URL
```

## Building

Please use the Nix package manager if possible,
it makes everything way, way easier.

Alternatively if you don't want to build the project yourself, 
you can just use a prebuilt Docker image, see [here](https://github.com/jiribenes/lily/releases/tag/v0.1.0.0).

### Nix

```
nix-build --attr lily default.nix
```

The resulting executable is located in `result/bin/`.
If you wish to install it onto your system using Nix,
use `nix-env -f default.nix -iA lily`.

You may also build a more static version of the executable by using `static-exe`
instead of `lily` in the commands above.

#### Developing with Nix

Use `nix-shell shell.nix` to create a development shell --
a shell with all development dependencies such as Cabal, GHC, etc.

You can use normal Cabal commands when in Nix shell
to work on Lily: 

* To build Lily using Cabal (better for development, incremental builds):
```
cabal new-build lily
```

* To build and run Lily using Cabal with some arguments in `$ARGS`:
```
cabal new-run lily -- $ARGS
```

* To open a GHCi REPL using Cabal:
```
cabal new-repl lily
```

#### Updating dependenies using Niv

The [niv](https://github.com/nmattia/niv) tool is used to
manage the version of nixpkgs. It is currently pinned
to a version of nixpkgs for which lily builds and works.

To update the version of nixpkgs, use the command `niv update`
in the development nix-shell.

#### Creating and using a Docker image with Nix

Requires Nix, Docker.
Warning: Creates a ~300MB image.

1. Create and load the image using Nix
```
docker load -i $(nix-build --attr docker default.nix)
```

2. Run the image in current directory
```
docker run -itv $PWD:/data lily-docker lily [ARGS...]
```

_Example usage:_
```
docker run -itv $PWD:/data lily-docker lily lint examples/various.cpp
```

### Cabal/Stack

Seek the [Cabal CI workflow](https://github.com/jiribenes/lily/blob/master/.github/workflows/cabal.yml) in this repository for guidance.

1) Ensure that your GHC version is 8.8.3
2) Ensure that you have Clang installed with libraries
3) Install my [clang-pure fork](https://github.com/jiribenes/clang-pure) located in the `deps/` folder using _Cabal/Stack_. For more instructions, view the file `DEV.md` in `deps/clang-pure`. Ideally it should be built by `cabal new-build` in the main folder but who knows?
4) Install this project using _Cabal/Stack_.
5) Pray that everything worked.

## Using

Assuming that `$LILY` is the way to invoke Lily
as it might depend on which installation was chosen.

Use the following command to lint an example file `examples/doublemove.cpp`:
```
$LILY lint examples/doublemove.cpp 
```

## Extending

A common issue might be assigning a type to an expression from some C++ library.
In order to add your own annotations, modify the `initialInferState` function in `src/Type/Infer.hs`.
Note that `std::move` is there as a functioning example of a user-added annotation.
