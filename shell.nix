let
  nixpkgs = import ./nix/packages;
  package = import ./.;
in
  package.env
