{ mkDerivation, ansi-wl-pprint, base, bytestring, containers
, hashable, hspec, mtl, parsec, parsers, QuickCheck, raw-strings-qq
, regex-posix, split, stdenv, template-haskell, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "inline-c";
  version = "0.9.1.0";
  sha256 = "829677f0a781431a55f870cef405e3219fb16842c9c1ab265ddc35890b8b2976";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base bytestring containers hashable mtl parsec
    parsers template-haskell transformers unordered-containers vector
  ];
  testHaskellDepends = [
    ansi-wl-pprint base containers hashable hspec parsers QuickCheck
    raw-strings-qq regex-posix split template-haskell transformers
    unordered-containers vector
  ];
  description = "Write Haskell source files including C code inline. No FFI required.";
  license = stdenv.lib.licenses.mit;
}
