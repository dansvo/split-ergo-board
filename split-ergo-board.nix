{ mkDerivation, base, HUnit, OpenSCAD, QuickCheck, stdenv }:
mkDerivation {
  pname = "split-ergo-board";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base HUnit OpenSCAD QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
