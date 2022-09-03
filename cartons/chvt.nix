{ config, pkgs, ... }:

let

  inherit (pkgs.stdenv)
    shell
  ;

  uid = 493;
  gid = 496;

  chvt-priv = pkgs.callPackage ./pkgs/custom/chvt-priv.nix {
    inherit uid gid;

    chvtPath = "/run/wrappers/bin/chvt-wrapped";
  };

in {
  nixpkgs.overlays = [
    (self: super: {
      inherit chvt-priv;
    })
  ];

  environment.systemPackages = [ pkgs.chvt-priv ];

  system.activationScripts = {
    chvt-sec =
      let setfacl = "${pkgs.acl}/bin/setfacl";
      in {
        text = ''
          ${setfacl} -m u:chvt:r--,m::rwx /dev/console
          ${setfacl} -m u:chvt:r--,m::rwx /dev/tty
          ${setfacl} -m u:chvt:r--,m::rwx /dev/tty0
        '';
        deps = [];
      }
    ;
  };

  users = {
    users.chvt = {
      isSystemUser = true;
      inherit uid;
    };

    groups.chvt = {
      inherit gid;
    };
  };

  security.wrappers = {
    chvt-wrapped = {
      source = "${pkgs.kbd}/bin/chvt";
      owner = "root";
      group = "chvt";
      capabilities = "cap_sys_tty_config+ep";
      permissions = "0550";
    };

    chvt = {
      source = "${pkgs.chvt-priv}/bin/chvt-priv";
      owner = "root";
      group = "chvt";
      setuid = true;
      permissions = "a+rwx,u-w,g-w,o-rwx"; #0550
    };
  };
}
