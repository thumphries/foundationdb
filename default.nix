{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  fdb = nixpkgs.pkgs.foundationdb;
in
  nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./foundationdb-hs.nix {
    fdb_c = fdb;
  }
