import ./nixpkgs {
  config = {
    packageOverrides = nixpkgs: {
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = self: super: {
          inherit (import ./squeal nixpkgs self super) squeal-postgresql;
          text-builder = nixpkgs.haskell.lib.dontCheck super.text-builder;
        };
      };
    };
  };
}
