{
  leo = { home, user, group }: [
    # ssh
    { mkdir = "${home}/.ssh"; }

    {
      name = "${home}/.ssh/config";
      rCTarget = "${home}/.ssh/config";
      text = ''
        Match User leo-github, Host localhost
          IdentityFile /home/leo/.ssh/leo-github-private-key-ed25519

        Host github.com
          User git
          ProxyCommand ssh -T -A leo-github@localhost
      '';

      inherit user group;
    }

    {
      passName = "buriti/bastion/leo-github/ssh/private-key/ed25519";
      rCTarget = "${home}/.ssh/leo-github-private-key-ed25519";

      inherit user group;
    }

    # gitconfig
    {
      passName = "buriti/user/leo/dotfile/gitconfig";
      rCTarget = "${home}/.gitconfig";

      inherit user group;

      permissions = "0644";
    }

    { backupAclsFoot = "${home}"; }
    { backupAclsHead = "${home}/.password-store"; }
    { backupAclsHead = "${home}/.gnupg"; }
  ];

  leo-github = { home, user, group }: [
    {
      mkdir = home;
      inherit user group;
    }

    # ssh
    {
      mkdir = "${home}/.ssh";
      inherit user group;
    }

    {
      name = "${home}/.ssh/config";
      rCTarget = "${home}/.ssh/config";
      text = ''
        Host github.com
          IdentityFile ${home}/.ssh/private-key-ed25519
      '';

      inherit user group;
    }

    {
      passName = "github/leosbotelho/ssh/private-key/ed25519";
      rCTarget = "${home}/.ssh/private-key-ed25519";

      inherit user group;
    }

    # mfa
    {
      passName = "buriti/bastion/leo-github/dotfile/google-authenticator";
      rCTarget = "${home}/.google_authenticator";

      inherit user group;

      permissions = "0400";
    }
  ];

  leo-buku = { home, user, group }: [
    {
      mkdir = home;
      inherit user group;
    }

    {
      backupAclsFoot = "${home}";
      backupAclsHead = "${home}/.local/share/buku/bookmarks.db";
    }
  ];
}
