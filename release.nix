let
  pkgs = import <nixpkgs> { config = {allowBroken = true;}; };
  stdenv = pkgs.stdenv;
  split_ergo_board =
    let
      config = {
        allowBroken = true;

        packageOverrides = pkgs: rec {
          haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              split_ergo_board =
                haskellPackagesNew.callPackage ./split-ergo-board.nix { };
              OpenSCAD =
                haskellPackagesNew.callPackage ./OpenSCAD.nix { };
              QuickCheck =
                haskellPackagesNew.callPackage ./QuickCheck.nix { };
              base =
                haskellPackagesNew.callPackage ./base.nix { };
            };
          };
        };
      };

      pkgs = import <nixpkgs> { inherit config; };

in
  pkgs.haskellPackages.split_ergo_board;

in
  {
  scad_files = pkgs.stdenv.mkDerivation {
    name = "kbd_scad_files";
    inherit split_ergo_board;
    builder = builtins.toFile "builder.sh"
      ''
        source $stdenv/setup
        mkdir $out
        cd $out
        $split_ergo_board/bin/split_ergo_board
      '';
  };
  }
