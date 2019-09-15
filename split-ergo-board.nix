{ mkDerivation, base, implicit, stdenv }:
  
mkDerivation {
  pname = "split_ergo_board";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base implicit ];
  license = stdenv.lib.licenses.bsd3;
}
