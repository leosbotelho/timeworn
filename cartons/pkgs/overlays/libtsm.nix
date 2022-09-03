self: super:

let

  cfgPath = ../config/libtsm;

in {
  libtsm = super.libtsm.overrideAttrs (oldAttrs: {
    patches = oldAttrs.patches ++ [
      (cfgPath + "/libtsm-3-buriti-colors.diff")
    ];
  });
}
