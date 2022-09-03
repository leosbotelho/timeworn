self: super:

let

  customPkgsPath = ../custom;

  customScriptPkg = pkg: super.callPackage (customPkgsPath + pkg) {};

in {
  power-stats = customScriptPkg "/power-stats.nix";
  refresh-power-stats = customScriptPkg "/refresh-power-stats.nix";
  x-notify-on-low-battery = customScriptPkg "/x-notify-on-low-battery.nix";
}
