let
  pkgs = import <nixpkgs> { config = {allowBroken = true;}; };
  stdenv = pkgs.stdenv;
  split_ergo_board = pkgs.haskellPackages.callPackage ./split-ergo-board.nix { };

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
