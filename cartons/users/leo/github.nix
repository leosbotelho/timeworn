{ lib, ... }:

{
  imports = [ ../groups/bastions.nix ];

  users.users.leo-github = {
    uid = 1002;
    group = "bastions";

    isNormalUser = true;

    openssh.authorizedKeys.keys =
      let publicKey = builtins.extraBuiltins.pass
                        "buriti/bastion/leo-github/ssh/public-key/ed25519";

          options =
              ''no-pty,''
            + ''command="ssh-add ~/.ssh/private-key-ed25519''
            +  '' && socat - TCP:github.com:22" '';

      in lib.mkOverride 0 [
        (options + publicKey)
      ];

    home = "/bastions/leo-github";
    createHome = false;

    extraGroups = [ "keys" ];
  };
}
