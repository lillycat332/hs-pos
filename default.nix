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
