{ config, options, pkgs, ... }:

let

  env' =
    config.services.xserver.displayManager.job.environment;

  slimCfgFile = env'.SLIM_CFGFILE;
  slimThemesDir = env'.SLIM_THEMESDIR;

  user = "leo";
  inherit (config.users.users.${user})
    home uid
  ;
  gid = 497;

  tty = toString (
       config.services.xserver.tty
    or options.services.xserver.tty.default
  );

  env = [
    "SLIM_CFGFILE=${slimCfgFile}"
    "SLIM_THEMESDIR=${slimThemesDir}"
    "USER=${user}"
    "HOME=${home}"
  ];

  slimlock-priv = pkgs.callPackage ./pkgs/custom/slimlock-priv.nix {
    inherit uid gid env;

    slimlockPath = "/run/wrappers/bin/slimlock-wrapped";
  };

  lock = pkgs.writeScriptBin "lock" ''
    #!${pkgs.stdenv.shell}

    chvt ${tty}
    slimlock
  '';

in {
  imports = [
    ./chvt.nix
  ];

  nixpkgs.overlays = [
    (self: super: {
      inherit slimlock-priv;
    })
  ];

  environment.systemPackages = [
    pkgs.slim
    pkgs.slimlock-priv
    lock
  ];

  services.xserver.displayManager = {
    slim = {
      enable = true;
      defaultUser = user;
    };
  };

  users.groups.slimlock = {
    inherit gid;
  };

  system.activationScripts = {
    slim-sec = {
      text = ''
        ${pkgs.acl}/bin/setfacl -m g:slimlock:rw-,m::rwx /dev/console
      '';
      deps = [];
    };
  };

  security.wrappers = {
    slimlock-wrapped = {
      source = "${pkgs.slim}/bin/slimlock";
      owner = "root";
      group = "slimlock";
      capabilities = "cap_sys_tty_config+ep";
      permissions = "0550";
    };

    slimlock = {
      source = "${pkgs.slimlock-priv}/bin/slimlock-priv";
      owner = "root";
      group = "slimlock";
      setuid = true;
      permissions = "a+rwx,u-w,g-w,o-rwx"; #0550
    };
  };

  systemd.services.lock-on-sleep = {
    description = "Locks X session on sleep";

    before = [ "sleep.target" ];
    after = [ "graphical-session.target" ];
    wantedBy = [ "sleep.target" ];

    serviceConfig = {
      ExecStartPre = "${pkgs.chvt-priv}/bin/chvt-priv ${tty}";
      ExecStart = ''${pkgs.slimlock-priv}/bin/slimlock-priv'';
    };
  };
}
