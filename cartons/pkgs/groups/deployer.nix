{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nixops
    nix-plugins
    gnupg
    pass
    srm
    apg
  ];
}
