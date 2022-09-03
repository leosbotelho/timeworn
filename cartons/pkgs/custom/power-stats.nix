{ pkgs }:

pkgs.writeScriptBin "power-stats" ''
  #!${pkgs.stdenv.shell}

  ${pkgs.upower}/bin/upower -i "$*"
''
