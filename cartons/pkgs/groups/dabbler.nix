{ pkgs, ... }:

{
  imports = [
    ../wrappers/radare2.nix
    ../wrappers/ghidra.nix
  ];

  environment.systemPackages = with pkgs; [
    wireshark-cli
    zap
    spiped
    socat
    httrack
    httpie
    fortune
    sl
    figlet
    shellcheck
    gcc
    binutils
    ltrace
    strace
    lynx
    macchanger
    xxd
  ];
}
