let
  nixpkgs = import ./nix/packages;
  package = import ./.;
in
  nixpkgs.lib.overrideDerivation package.env (drv: {
    nativeBuildInputs =
      drv.nativeBuildInputs ++
      [ nixpkgs.postgresql ];
    shellHook = drv.shellHook + "
      mkdir -p $PWD/database/pgdata
      export PGDATA=$PWD/database/pgdata

      mkdir -p $PWD/logs
    ";
  })
