let
  pkgs = import <nixpkgs> { };
  stdenv = pkgs.llvmPackages_13.stdenv;
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    clang
    zlib
    cabal-install
    haskellPackages.Cabal_3_8_1_0
    haskell.compiler.ghc924
    sqlite
    postgresql
  ];
}
