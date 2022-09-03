{ pkgs, ... }:

let

  ghidra = pkgs.callPackage ../custom/ghidra.nix {};

in {
  environment.systemPackages = [ ghidra ];
}
