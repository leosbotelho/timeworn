# Copied - and modified - for more granularity/control

{ lib, config, pkgs, ... }:

let

  inherit (lib)
    listToAttrs concatMap
  ;

  inherit (config.services.xserver)
    layout xkbVariant xkbOptions
  ;

  cmdOpts = builtins.concatStringsSep " ";

  extraConfig = ''
    font-name=Monospace
    font-size=15
  '';

  extraOptions = cmdOpts [
   "--xkb-layout '${layout}'"
   "--xkb-variant '${xkbVariant}'"
   "--xkb-options '${xkbOptions}'"
  ];

  configDir = pkgs.writeTextFile {
    name = "kmscon-config";
    destination = "/kmscon.conf";
    text = extraConfig;
  };

  kmsconCmd =
     ''${pkgs.kmscon}/bin/kmscon "--vt=%I" ${extraOptions} ''
    +''--seats=seat0 --no-switchvt --configdir ${configDir} ''
    +''--login -- ${pkgs.shadow}/bin/login''
  ;

  kmsconUnit = n:
    pkgs.runCommand "unit" { preferLocalBuild = true; } ''
      mkdir -p $out
      ln -s \
        ${config.systemd.units."kmsconvt@.service".unit}/kmsconvt@.service \
        $out/autovt@${n}.service
    ''
  ;

  kmsconAutovt = ns:
    let f = n: [
      {
        name = "autovt@${n}.service";
        value.unit = kmsconUnit n;
      }
      {
        name = "getty@${n}.service";
        value.enable = false;
      }
    ];

    in listToAttrs (concatMap f ns)
  ;

in {
  systemd.units = {
    # Largely copied from unit provided with kmscon source
    "kmsconvt@.service".text = ''
      [Unit]
      Description=KMS System Console on %I
      Documentation=man:kmscon(1)
      After=systemd-user-sessions.service
      After=plymouth-quit-wait.service
      After=systemd-logind.service
      After=systemd-vconsole-setup.service
      Requires=systemd-logind.service
      Before=getty.target
      Conflicts=getty@%i.service
      OnFailure=getty@%i.service
      IgnoreOnIsolate=yes
      ConditionPathExists=/dev/tty0
      [Service]
      ExecStart=
      ExecStart=${kmsconCmd}
      UtmpIdentifier=%I
      TTYPath=/dev/%I
      TTYReset=yes
      TTYVHangup=yes
      TTYVTDisallocate=yes
      X-RestartIfChanged=false
    '';
  } // kmsconAutovt [ "tty1" "tty2" "tty3" ];

  systemd.services.systemd-vconsole-setup.enable = false;
}
