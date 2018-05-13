{ mkDerivation, base, bytestring, c2hs, directory, fdb_c, filepath
, process, QuickCheck, stdenv, temporary, vector
}:
mkDerivation {
  pname = "foundationdb";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base bytestring vector ];
  librarySystemDepends = [ fdb_c ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [
    base directory filepath process QuickCheck temporary
  ];
  homepage = "https://github.com/thumphries/foundationdb";
  description = "Haskell bindings to Apple's FoundationDB";
  license = stdenv.lib.licenses.bsd3;
}
