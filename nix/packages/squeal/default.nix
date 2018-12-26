{ lib
, fetchFromGitHub
, haskell
, ...
}:
self:
super:
let
  squeal-source = fetchFromGitHub {
    owner = "morphismtech";
    repo = "squeal";
    rev = lib.fileContents ./rev;
    sha256 = lib.fileContents  ./sha;
  };
in
  {
    squeal-postgresql = haskell.lib.dontCheck (self.callPackage (self.haskellSrc2nix {
      name = "squeal-postgresql";
      src = "${squeal-source}/squeal-postgresql";
    }) {});
  }
