let
  pkgs = import <nixpkgs> { };
  stdenv = pkgs.llvmPackages_13.stdenv;
in

{ mkDerivation
, aeson
, base
, beam-core
, beam-migrate
, beam-sqlite
, HDBC
, HDBC-sqlite3
, http-types
, lib
, scotty
, sqlite-simple
, text
, wai
, wai-extra
, wai-middleware-static
}:
let
  pkgs = import <nixpkgs> { };
  stdenv = pkgs.llvmPackages_13.stdenv;
in
mkDerivation {
  pname = "hs-pos";
  version = "0.1.0.0";
  src = /devel/hs-pos;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    beam-core
    beam-migrate
    beam-sqlite
    HDBC
    HDBC-sqlite3
    http-types
    scotty
    sqlite-simple
    text
    wai
    wai-extra
    wai-middleware-static
  ];
  license = lib.licenses.bsd3;
}

# pkgs.stdenv.mkDerivation {
#   name = "hs-pos";
#   version = "0.1.0";

#   # src = pkgs.fetchFromGitHub {
#   #   owner = "lillycat332";
#   #   repo = "hs-pos";
#   #   rev = "master";
#   #   hash = "";
#   # };

#   buildInputs = with pkgs; [
#     clang
#     zlib
#     cabal-install
#     ghc
#     sqlite
#   ];

#   buildPhase = ''
#     cabal build
#   '';

#   meta = {
#     description = "PoS (point of sale) system in haskell";
#     # homepage = "https://github.com/lillycat332/hs-pos";
#     license = "BSD";
#   };
# }

