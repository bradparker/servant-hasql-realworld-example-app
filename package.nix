{ mkDerivation, base, bytestring, containers, contravariant, hasql
, lens, network-uri, process, squeal-postgresql, stdenv, text, time
}:
mkDerivation {
  pname = "servant-hasql-realworld-example-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers contravariant hasql lens
    squeal-postgresql text time
  ];
  executableHaskellDepends = [
    base bytestring hasql network-uri process squeal-postgresql
  ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}
