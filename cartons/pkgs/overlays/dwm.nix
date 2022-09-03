self: super:

let

  cfgPath = ../config/dwm;

in {
  dwm = (super.dwm.override {
    patches = [
      (cfgPath + "/dwm-fakefullscreen-20170508-ceac8c9.diff")
      (cfgPath + "/dwm-noborder-6.2.diff")
      (cfgPath + "/dwm-pertag-20170513-ceac8c9.diff")
      (cfgPath + "/dwm-bottomstack-20160719-56a31dc.diff")
      (cfgPath + "/config-dwm-6.2.diff")
    ];
  }).overrideAttrs (oldAttrs: {
    # required for the current dwm config patch
    propagatedBuildInputs = [
      self.dmenu
      self.xterm
      self.jq
      self.firefox
      self.autorandr
      # alsaUtils
      # systemd
      # redshift
    ];
  });
}
