# { pkgs, lib, stdenv, fetchFromGitHub }:
let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.llvmPackages_13.stdenv;
in
pkgs.stdenv.mkDerivation {
  name = "hs-pos";
  version = "0.1.0";

  # src = pkgs.fetchFromGitHub {
  #   owner = "lillycat332";
  #   repo = "meowlang";
  #   rev = "master";
  #   hash = "sha256-lUG1yufj8OkN/Ycy2h7VoCNgAaTaLGH8sAW7+IGuXiQ=";
  # };

  buildInputs = with pkgs; [
		clang
		zlib
		cabal-install
		ghc 
	];

  installPhase = ''
    cabal build
  '';

  meta = {
    description = "CUM";
    # homepage = "https://github.com/lillycat332/meowlang";
    license = "BSD";
  };
}