{ pkgs, ... }:

{
  imports = [
    ./fzf.nix
  ];

  environment.systemPackages = with pkgs; [
    neovim
    ranger
  ];
}
