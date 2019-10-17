{ mkDerivation, aeson, aeson-pretty, base, binary, bytestring
, cookie, hpack, hspec, hspec-wai, http-conduit, http-types, jose
, lens, lucid, monad-time, mtl, network-uri, optparse-applicative
, process, scotty, stdenv, tasty, tasty-ant-xml, tasty-hspec, text
, time, unordered-containers, wai, wai-extra, wai-middleware-static
, warp
}:
mkDerivation {
  pname = "login-with-okta";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base binary bytestring cookie http-conduit
    http-types jose lens lucid monad-time mtl network-uri
    optparse-applicative scotty text time unordered-containers wai
    wai-extra wai-middleware-static warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [
    aeson base bytestring hspec hspec-wai http-types jose lens lucid
    process tasty tasty-ant-xml tasty-hspec text wai wai-extra
  ];
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/freizl/haskell-okta#readme";
  license = stdenv.lib.licenses.asl20;
}
