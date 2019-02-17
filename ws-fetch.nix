{ mkDerivation, aeson, base, binary, bytestring, case-insensitive
, hpack, http-client, http-client-tls, http-types, lens, MissingH
, stdenv, text, unordered-containers, wai, wai-websockets, warp
, websockets, wreq, yaml, configureFlags
}:
mkDerivation {
  pname = "ws-fetch";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    aeson base binary bytestring case-insensitive http-client
    http-client-tls http-types lens MissingH text unordered-containers
    wai wai-websockets warp websockets wreq
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base binary bytestring case-insensitive http-client
    http-client-tls http-types lens MissingH text unordered-containers
    wai wai-websockets warp websockets wreq yaml
  ];
  testHaskellDepends = [
    aeson base binary bytestring case-insensitive http-client
    http-client-tls http-types lens MissingH text unordered-containers
    wai wai-websockets warp websockets wreq
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/Lupino/ws-fetch#readme";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = configureFlags;
}
