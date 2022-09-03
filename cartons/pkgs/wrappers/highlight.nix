{ pkgs, ... }:

let

  cfgPath = ../config/highlight;

  defaultsOverride = pkgs.writeText "defaults-override.lua" ''
    ${builtins.readFile (cfgPath + "/defaults-override.lua")}
  '';

  highlight =
    pkgs.symlinkJoin {
      name = "highlight";
      paths = [ pkgs.highlight ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        cp ${defaultsOverride} $out/share/highlight/plugins/defaults-override.lua

        wrapProgram $out/bin/highlight \
          --add-flags "--plug-in=\"defaults-override\" --data-dir=\"$out/share/highlight\""
      '';
    };

in {
  environment.systemPackages = [ highlight ];

  programs.bash.shellAliases = {
    hat = "highlight -O xterm256 --force";
  };
}
