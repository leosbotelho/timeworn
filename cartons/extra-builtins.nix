let

  lib = import <nixpkgs/lib>;

in { exec, ... }: rec {
  pass = name: exec [./nix-pass.sh name];

  inherit (import ./lib/keys-and-adhoc-persistence.nix { inherit lib pass; })
    arbitrateKeysAndAdhocPersistence
  ;
}
