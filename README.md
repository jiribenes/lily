# Lily

## Downloading the repository

```
git clone --recursive-submodules $REPO_URL
```

## Building

Please use the Nix package manager if possible,
it makes everything way, way easier.

### Nix

```
nix-build release.nix
```

The resulting executable is located in `result/bin/`.
If you wish to install it onto your system, use `nix-env -f release.nix -iA lily`.

### Cabal/Stack

You have been warned.

1) Ensure that your GHC version is either 8.6.5, 8.8.3 or 8.10.1
2) Install my [clang-pure fork](https://github.com/jiribenes/clang-pure) located in the `deps/` folder using _Cabal/Stack_. For more instructions, view the file `DEV.md` in `deps/clang-pure`.
3) Install this project using _Cabal/Stack_.
4) Pray that everything worked.
