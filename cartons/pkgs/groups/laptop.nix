{ pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ../../suspend-on-low-battery.nix
  ];

  environment.systemPackages = with pkgs; [
    upower

    power-stats
    refresh-power-stats
  ];

  powerManagement.cpuFreqGovernor = pkgs.lib.mkDefault "powersave";

  services.xserver.libinput = {
    enable = true;

    tapping = true;

    naturalScrolling = false;
    middleEmulation = true;
  };

  services.upower.enable = true;
  powerManagement.enable = true;
  services.logind.extraConfig = ''
    HandlePowerKey=suspend
    HandleSuspendKey=suspend
    HandleHibernateKey=suspend
    HandleLidSwitch=suspend
    HandleLidSwitchDocked=ignore
  '';

  programs.bash.shellAliases = {
    fzf-power-device = "upower -e | fzf";

    fzf-power-stats = ''power-stats "$(fzf-power-device)"'';
    fzf-refresh-power-stats = ''refresh-power-stats "$(fzf-power-device)"'';
  };
}
