{ pkgs, ... }:

{
  users.users.leo = {
    uid = 1000;

    isNormalUser = true;

    hashedPassword =
      builtins.extraBuiltins.pass "buriti/user/leo/pass-linux-sha512";

    extraGroups = [
      "keys" "slimlock" "chvt"
    ];

    shell = pkgs.bashInteractive;
  };
}
