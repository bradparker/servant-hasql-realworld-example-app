let
  nixpkgs = import ./nix/packages;
  tools =
    [
      nixpkgs.direnv
      nixpkgs.postgresql
    ];

  package = import ./.;
  packageWithTools =
    nixpkgs.haskell.lib.addBuildDepends package tools;
in
  nixpkgs.lib.overrideDerivation packageWithTools.env (drv: {
    shellHook = drv.shellHook + "
      mkdir -p $PWD/.dev/pgdata
      export PGDATA=$PWD/.dev/pgdata

      mkdir -p $PWD/.dev/logs
    ";
  })
