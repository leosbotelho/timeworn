{
  root = { home, user, group }: [
    # ssh
    { mkdir = "${home}/.ssh"; }

    {
      passName = "buriti/user/root/ssh/config";
      rCTarget = "${home}/.ssh/config";
    }

    {
      passName = "buriti/user/root/ssh/private-key/ed25519";
      rCTarget = "${home}/.ssh/root-private-key-ed25519";
    }

    # nixops
    { mkdir = "${home}/.nixops"; }
    { mkdir = "${home}/.nixops/personal"; }

    # cachix
    { mkdir = "${home}/.config"; }
    { mkdir = "${home}/.config/cachix"; }

    {
      passName = "cachix/leo/cachix-dhall";
      rCTarget = "${home}/.config/cachix/cachix.dhall";
    }

    { backupAclsFoot = "${home}"; }
    { backupAclsHead = "${home}/.password-manager"; }
    { backupAclsHead = "${home}/.gnupg"; }
    { backupAclsHead = "${home}/.nixops"; }

    { mkdir = "/bastions"; permissions = "0755"; }
  ];

  inherit (import ./leo.nix)
    leo leo-github leo-buku
  ;
}
