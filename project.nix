{ mkDerivation, aeson, base, dhall, foreign-store, hpack, hspec
, http-client, http-types, mtl, postgresql-simple, safe-exceptions
, say, servant, servant-client, servant-flatten, servant-server
, stdenv, stm, text, uuid, warp
}:
mkDerivation {
  pname = "weather-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base dhall foreign-store mtl postgresql-simple
    safe-exceptions say servant servant-flatten servant-server text
    uuid warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base mtl postgresql-simple servant servant-flatten
    servant-server text uuid warp
  ];
  testHaskellDepends = [
    aeson base hspec http-client http-types mtl postgresql-simple
    servant servant-client servant-flatten servant-server stm text uuid
    warp
  ];
  doCheck = false;
  prePatch = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
