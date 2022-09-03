{ hostnames }:

let

  binaryCaches = import ../binary-caches.nix;

in {
  defaults = { name, lib, options, pkgs, ... }: {
    deployment.targetHost = hostnames.${name} or "${name}.invalid";

    environment.etc.nixpkgs.source = lib.cleanSource ../nixpkgs;

    nix = {
      nixPath = [
        "nixpkgs=/etc/nixpkgs"
      ];

      binaryCaches = with binaryCaches; [
        cachix.url
        hydra-iohk.url
        leosbotelho.url
      ];

      binaryCachePublicKeys = with binaryCaches; [
        cachix.publicKey
        hydra-iohk.publicKey
        leosbotelho.publicKey
      ];
    };

    imports = [
      ../nixops-managed.nix
      ../overlays-compat.nix
      ../users/backup.nix
    ];
  };
}
