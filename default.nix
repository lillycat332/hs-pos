{ mkDerivation
, aeson
, base
, bcrypt
, bytestring
, convertible
, directory
, hashable
, HDBC
, HDBC-postgresql
, HDBC-sqlite3
, http-types
, lib
, optparse-applicative
, random
, scotty
, text
, time
, tuple
, uuid
, vector
, wai
, wai-extra
, wai-middleware-static
}:
mkDerivation {
  pname = "hs-pos";
  version = "2.1.0.0";
  src = /devel/hs-pos;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    bcrypt
    bytestring
    convertible
    directory
    hashable
    HDBC
    HDBC-postgresql
    HDBC-sqlite3
    http-types
    optparse-applicative
    random
    scotty
    text
    time
    tuple
    uuid
    vector
    wai
    wai-extra
    wai-middleware-static
  ];
  executableHaskellDepends = [
    aeson
    base
    convertible
    HDBC-postgresql
    HDBC-sqlite3
    http-types
    optparse-applicative
    scotty
    text
    wai
    wai-extra
    wai-middleware-static
  ];
  homepage = "https://github.com/lillycat332/hs-pos";
  description = "Point of Sale system backend";
  license = lib.licenses.bsd3;
  mainProgram = "hs-pos";
}
