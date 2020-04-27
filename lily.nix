{ mkDerivation, base, bytestring, clang-pure, containers, lens, mtl
, stdenv, text, transformers
}:
mkDerivation {
  pname = "lily";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring clang-pure containers lens mtl text transformers
  ];
  executableHaskellDepends = [
    base bytestring clang-pure containers lens mtl text transformers
  ];
  testHaskellDepends = [
    base bytestring clang-pure containers lens mtl text transformers
  ];
  description = "C++ linter based on linear types";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
