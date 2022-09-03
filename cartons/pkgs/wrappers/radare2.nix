{ pkgs, ... }:

let

  radare2DotfilesPath = ../config/radare2;

  radare2DotfilesStorePath = pkgs.runCommand "radare2" {} ''
    cp -r ${radare2DotfilesPath} $out
    substituteInPlace $out/radare2rc --subst-var-by buritiCo "$out/buriti"
  '';

  radare2rcFile = radare2DotfilesStorePath + "/radare2rc";

  radare2 =
    pkgs.symlinkJoin {
      name = "radare2";
      paths = [ pkgs.radare2 ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/radare2 --add-flags \
          "-i ${radare2rcFile}"
      '';
    };

in {
  environment.systemPackages = [ radare2 ];
}
