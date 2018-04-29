{ mkDerivation, base, bytestring, c2hs, fdb_c, QuickCheck, stdenv
, vector
}:
mkDerivation {
  pname = "foundationdb";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base bytestring vector ];
  librarySystemDepends = [ fdb_c ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [ base QuickCheck ];
  homepage = "https://github.com/thumphries/foundationdb";
  description = "Haskell bindings to Apple's FoundationDB";
  license = stdenv.lib.licenses.bsd3;
}
