netArgs@{ hostnames }:

let

  inherit (builtins.extraBuiltins)
    arbitrateKeysAndAdhocPersistence
  ;

in {
  network = {
    enableRollback = true;
  };

  inherit (import ./defaults.nix netArgs)
    defaults
  ;

  buriti = args@{ lib, config, ... }:
  let

    inherit (
      arbitrateKeysAndAdhocPersistence
        config.users.users
        (import ../keys/buriti.nix)
        [ "root" "leo" "leo-buku" "leo-github" ]
    )
      deploymentKeys systemdTmpfilesRules
    ;
  in {
    imports = [
      <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
      ../buriti.nix

      ../disable-su-to-root.nix
      ../openssh.nix
      ../pkgs/groups/deployer.nix
    ];

    fileSystems = {
      "/" = {
        device = "/dev/disk/by-uuid/2db15e4d-024b-4367-8427-73a12aea889c";
        fsType = "ext4";
      };
      "/boot" = {
        device = "/dev/disk/by-uuid/7347-F55D";
        fsType = "vfat";
      };
    };

    swapDevices = [
      { device = "/dev/disk/by-uuid/30ac63c4-265f-4ae8-9c66-df40e607a335"; }
    ];

    nix.maxJobs = lib.mkDefault 4;

    # No encryption, use judiciously
    boot = {
      initrd.availableKernelModules = [
        "xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod"
      ];
      loader = {
        systemd-boot = {
          enable = true;
        };
      };
      kernelModules = [ "kvm-intel" ];

      tmpOnTmpfs = true;
    };

    security.sudo.enable = false;

    users.mutableUsers = false;

    deployment.keys = deploymentKeys;

    systemd.tmpfiles.rules = systemdTmpfilesRules;

    programs.ssh = {
      startAgent = true;
      agentTimeout = "30s";
    };
  };
}
