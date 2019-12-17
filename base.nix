{ mkDerivation, ghc-prim, integer-simple, rts, stdenv }:
mkDerivation {
  pname = "base";
  version = "4.7.0.2";
  sha256 = "57eafed0565a9edb4256e19329209f41df7dde01b6eae92b3f32abb2bd063727";
  libraryHaskellDepends = [ ghc-prim integer-simple rts ];
  doCheck = false;
  description = "Basic libraries";
  license = stdenv.lib.licenses.bsd3;
}
