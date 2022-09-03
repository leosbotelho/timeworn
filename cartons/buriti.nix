{ lib, pkgs, ... }:

let

  inherit (builtins.extraBuiltins) pass;

in {
  imports = [
    ./kmscon.nix
    ./kernel-vt-color-palette.nix
    ./dnscrypt-cisco-resolver.nix
    ./pkgs/groups/comprehensive.nix
    ./pkgs/groups/laptop.nix
    ./pkgs/groups/dev.nix
    ./pkgs/groups/dabbler.nix
    ./users/leo
    ./slim.nix
    ./chvt.nix
    ./x/keyboard/qwerty-intl-tupiniquim.nix
    ./x/notify-on-low-battery.nix
  ];

  nixpkgs.config.allowUnfree = true;

  hardware.cpu.intel.updateMicrocode = true;

  services.smartd.enable = true;

  users.users.root.shell = pkgs.bashInteractive;

  users.users.root.openssh.authorizedKeys.keys = lib.mkOverride 0 [
    (pass "buriti/user/root/ssh/public-key/ed25519")
  ];

  users.users.root.hashedPassword =
    pass "buriti/user/root/pass-linux-sha512";

  environment.pathsToLink = [ "/share" ];

  networking = {
    hostName = "buriti";

    wireless = {
      enable = true;
      networks =
        let
          inherit (import ./wlans.nix)
            wlan0
          ;
        in lib.mkMerge [
          wlan0
        ]
      ;
    };
  };

  time.timeZone = "America/Sao_Paulo";

  i18n.consoleUseXkbConfig = true;

  location = {
    # Goiania
    latitude = -16.686;
    longitude = -49.228;
  };
}
