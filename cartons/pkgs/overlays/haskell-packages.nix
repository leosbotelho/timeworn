self: super:

let
  customPkgPath = ../custom;

  hlib = super.haskell.lib;

  hpkgsPath = customPkgPath + "/haskell-packages";

in {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper:
      let pkg' = p: r: hlib.dontHaddock (hlib.dontCheck (hsuper.callPackage p r));
      in {
      };
  };
}
