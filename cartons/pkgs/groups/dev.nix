{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nasm
    openjdk11
    gdb
    # haskell
    cabal-install
    cabal2nix
    styx
    haskellPackages.brittany
    haskellPackages.stylish-haskell
    # idris
    haskellPackages.idris
  ];

  programs.bash.shellAliases = {
    bhs = "brittany";
    shs = "stylish-haskell";
  };
}
