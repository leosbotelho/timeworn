{ pkgs, ... }:

{
  imports = [
    ../wrappers/neovim.nix
    ../wrappers/fzf.nix
    ../wrappers/ripgrep.nix
    ../wrappers/highlight.nix
  ];

  environment.systemPackages = with pkgs; [
    smartmontools
    file
    fd
    ranger
    ncdu
    htop
    git
    tig
    stow
    tmux
    wget
    whois
    wol
    bashInteractive
  ];

  environment.variables.EDITOR = "nvim";
}
