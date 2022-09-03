{ lib, ... }:

let

  inherit (builtins)
    attrNames readDir
  ;

  overlaysPath = ./pkgs/overlays;

in {
  nixpkgs.overlays =
    map (file: import (overlaysPath + "/${file}"))
        (attrNames (readDir overlaysPath))
  ;

  nix.nixPath = [
    "nixpkgs-overlays=/run/current-system/overlays-compat/pkgs/overlays"
  ];

  system.extraSystemBuilderCmds = ''
    mkdir -p $out/overlays-compat/pkgs

    ln -sv ${./pkgs/config} $out/overlays-compat/pkgs/config
    ln -sv ${./pkgs/custom} $out/overlays-compat/pkgs/custom
    ln -sv ${./pkgs/overlays} $out/overlays-compat/pkgs/overlays
  '';
}
