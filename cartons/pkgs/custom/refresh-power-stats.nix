{ pkgs }:

pkgs.writeScriptBin "refresh-power-stats" ''
  #!${pkgs.stdenv.shell}

  ${pkgs.dbus}/bin/dbus-send --print-reply \
                             --system --dest=org.freedesktop.UPower \
                             "$*" org.freedesktop.UPower.Device.Refresh
''
