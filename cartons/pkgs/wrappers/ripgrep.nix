{ pkgs, ... }:

let

  ripgrep =
    pkgs.symlinkJoin {
      name = "ripgrep";
      paths = [ pkgs.ripgrep ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/rg \
          --add-flags "--colors='match:fg:blue' \
                       --colors='match:style:bold' \
                       --colors='path:bg:251' \
                       --colors='path:fg:blue'"
      '';
    };

in {
  environment.systemPackages = [ ripgrep ];
}
