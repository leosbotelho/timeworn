let

  dummyCfg = builtins.toFile "configuration.nix" ''
    assert builtins.trace "This is a NixOps managed machine" false;
    {}
  '';

in {
  nix.nixPath = [
    "nixos-config=${dummyCfg}"
  ];
}
