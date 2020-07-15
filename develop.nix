# For use with nix-shell
{ nixpkgs ? (import ./simplir/nixpkgs.nix {}) }:

with nixpkgs;
let
  trec-eval = callPackage ./simplir/trec-eval.nix {};
in stdenv.mkDerivation {
  name = "mediawiki-annotate-develop";
  buildInputs = [
    haskell.compiler.ghc883
    curl binutils-unwrapped leveldb
    zlib icu icu.out expat lzma trec-eval kyotocabinet
    postgresql ghc cabal-install
  ];
}
