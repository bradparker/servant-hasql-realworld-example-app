import ./nixpkgs {
  config = {
    packageOverrides = nixpkgs: {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = self: super: {
          squeal-postgresql = nixpkgs.haskell.lib.dontCheck super.squeal-postgresql;
          text-builder = nixpkgs.haskell.lib.dontCheck super.text-builder;
        };
      };
    };
  };
}
