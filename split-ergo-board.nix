{ mkDerivation, base, OpenSCAD, stdenv }:

mkDerivation {
  pname = "split_ergo_board";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base OpenSCAD ];
  license = stdenv.lib.licenses.bsd3;
}
