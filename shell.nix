let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.llvmPackages_13.stdenv;
in

pkgs.mkShell {
  buildInputs = with pkgs; [     
    clang
    zlib
    cabal-install
    haskell.compiler.ghc924
    sqlite 
    elmPackages.elm
  ];
}
