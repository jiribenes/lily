{ mkDerivation, base, clang-pure, stdenv }:
mkDerivation {
  pname = "lily";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base clang-pure ];
  executableHaskellDepends = [ base clang-pure ];
  testHaskellDepends = [ base clang-pure ];
  description = "C++ linter based on linear types";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
