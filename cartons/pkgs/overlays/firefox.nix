# github:Luis-Hebendanz/nix-configs/firefox.nix ack
# This might change soon

self: super:

let
  customPkgPath = ../custom;
  cfgPath = ../config/firefox;

  pkg = super.callPackage;

  https-everywhere = pkg (customPkgPath + "/https-everywhere.nix") {};
  ublock-origin = pkg (customPkgPath + "/ublock-origin.nix") {};

  wrapper  = pkg (customPkgPath + "/firefox-with-config.nix") {};

  firefox = wrapper super.pkgs.firefox-unwrapped {
    extraExtensions = [
      https-everywhere
      ublock-origin
    ];

    extraPolicies = {
      CaptivePortal = false;
    };

    disablePocket = true;
    disableFirefoxSync = true;
    allowNonSigned = false;
    clearDataOnShutdown = true;
    activateAntiTracking = true;
    disableTelemetry = true;
    disableGoogleSafebrowsing = true;
    dontCheckDefaultBrowser = true;

    extraPrefs = ''
      ${builtins.readFile (cfgPath + "/mozilla.cfg")}
    '';
  };

in {
  inherit firefox;
}
