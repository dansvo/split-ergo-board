{ mkDerivation, base, Cabal, colour, containers, deepseq, fetchgit
, filepath, HUnit, semigroups, stdenv, tasty, tasty-hunit, testpack
}:
mkDerivation {
  pname = "OpenSCAD";
  version = "0.3.0.2";
  src = fetchgit {
    url = "https://github.com/rybern/OpenSCAD.git";
    sha256 = "16nmsw3lirvp6k9ar5bxf7ip4cy5pyavwhkhd7z0ahbx5s7vsdq4";
    rev = "77ecaab281cef76f036f028562e81885d875aebb";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base colour containers filepath semigroups
  ];
  testHaskellDepends = [
    base Cabal colour containers deepseq filepath HUnit semigroups
    tasty tasty-hunit testpack
  ];
  homepage = "https://chiselapp.com/user/mwm/repository/OpenSCAD/";
  description = "ADT wrapper and renderer for OpenSCAD models";
  license = stdenv.lib.licenses.bsd3;
}
