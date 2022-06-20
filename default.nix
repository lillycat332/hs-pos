let
  pkgs = import <nixpkgs> { };
  stdenv = pkgs.llvmPackages_13.stdenv;
in

pkgs.stdenv.mkDerivation
{
  name = "hs-pos";
  version = "0.1.0";

  # src = pkgs.fetchFromGitHub {
  #   owner = "lillycat332";
  #   repo = "hs-pos";
  #   rev = "master";
  #   hash = "";
  # };

  buildInputs = with pkgs; [
    clang
    zlib
    cabal-install
    ghc
    sqlite
  ];

  buildPhase = ''
    cabal build
  '';

  meta = {
    description = "PoS (point of sale) system in haskell";
    homepage = "https://github.com/lillycat332/hs-pos";
    license = "BSD";
  };
}

