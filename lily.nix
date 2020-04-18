{ mkDerivation, base, clang-pure, containers, lens, stdenv }:
mkDerivation {
  pname = "lily";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base clang-pure containers lens ];
  executableHaskellDepends = [ base clang-pure containers lens ];
  testHaskellDepends = [ base clang-pure containers lens ];
  description = "C++ linter based on linear types";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
