# let
#   pkgs = import <nixpkgs> { };
#   stdenv = pkgs.llvmPackages_13.stdenv;
# in

# pkgs.stdenv.mkDerivation
# {
#   name = "hs-pos";
#   version = "0.1.0";

#   src =  ./.;
#   # pkgs.fetchFromGitHub {
#   #   owner = "lillycat332";
#   #   repo = "hs-pos";
#   #   rev = "master";
#   #   hash = "sha256-r2kPFE4u97G/PlC3wKu0JwZCq++iFM1bc3rNzhlxPIc=";
#   # };

#   buildInputs = with pkgs; [
#     clang
#     zlib
#     cabal-install
#     ghc
#     sqlite
#   ];

#   isLibrary = false;
#   isExecutable = true;

#   buildPhase = ''
#     cabal build
#   '';

#   meta = {
#     description = "PoS (point of sale) system in haskell";
#     homepage = "https://github.com/lillycat332/hs-pos";
#     license = lib.licenses.bsd3;
#   };
# }

{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-sqlite
, HDBC, HDBC-sqlite3, http-types, lib, optparse-applicative, scotty
, sqlite-simple, text, wai, wai-extra, wai-middleware-static, pkgs, ...
}:
mkDerivation {
  pname = "hs-pos";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base beam-core beam-migrate beam-sqlite HDBC HDBC-sqlite3
    http-types optparse-applicative scotty sqlite-simple text wai
    wai-extra wai-middleware-static
  ];
  buildInputs = with pkgs; [
    clang
    zlib
    cabal-install
    ghc
    sqlite
  ];
  license = lib.licenses.bsd3;
}
