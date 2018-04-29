{ mkDerivation, base, c2hs, fdb_c, hspec, stdenv }:
mkDerivation {
  pname = "foundationdb";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ fdb_c ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/thumphries/foundationdb";
  description = "Haskell bindings to Apple's FoundationDB";
  license = stdenv.lib.licenses.bsd3;
}
