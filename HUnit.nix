{ mkDerivation, base, call-stack, deepseq, filepath, stdenv }:
mkDerivation {
  pname = "HUnit";
  version = "1.6.0.0";
  sha256 = "7448e6b966e98e84b7627deba23f71b508e9a61e7bc571d74304a25d30e6d0de";
  libraryHaskellDepends = [ base call-stack deepseq ];
  testHaskellDepends = [ base call-stack deepseq filepath ];
  homepage = "https://github.com/hspec/HUnit#readme";
  description = "A unit testing framework for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
