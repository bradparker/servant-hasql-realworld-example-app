{ mkDerivation, base, bytestring, containers, contravariant, hasql
, neat-interpolation, raw-strings-qq, squeal-postgresql, stdenv
, text, time
}:
mkDerivation {
  pname = "servant-hasql-realworld-example-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers contravariant hasql neat-interpolation
    raw-strings-qq squeal-postgresql text time
  ];
  license = stdenv.lib.licenses.bsd3;
}
