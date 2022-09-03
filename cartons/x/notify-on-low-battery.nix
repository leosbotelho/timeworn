{ pkgs, ... }:

let

  x-notify-on-low-battery-script =
    pkgs.x-notify-on-low-battery + "/bin/x-notify-on-low-battery";

in {
  environment.systemPackages = [
    pkgs.x-notify-on-low-battery
  ];

  systemd.user.services.x-notify-on-low-battery = {
    description = "X low battery warnings";

    after = [ "upower.service" "graphical-session.target" ];
    partOf = [ "upower.service" "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart =
          "${x-notify-on-low-battery-script}"
        + " %h/.config/x-notify-on-low-battery-devices"
      ;
      Restart = "always";
      RestartSec = 20;
    };
  };
}
