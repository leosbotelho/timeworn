{
  imports = [ ../groups/bastions.nix ];

  users.users.leo-buku = {
    uid = 1001;
    group = "bastions";

    isNormalUser = true;

    hashedPassword =
      builtins.extraBuiltins.pass
        "buriti/bastion/leo-buku/pass-linux-sha512";

    home = "/bastions/leo-buku";
    createHome = false;
  };

  programs.bash.interactiveShellInit = ''
    buku() {
      su -c "buku $(echo "$@")" - leo-buku;
    }
  '';
}
