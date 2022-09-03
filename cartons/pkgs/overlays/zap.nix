self: super:

{
  zap =
    super.symlinkJoin {
      name = "zap";
      paths = [ super.zap ];
      nativeBuildInputs = [ super.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/zap \
          --set "_JAVA_AWT_WM_NONREPARENTING=1"
      '';
    }
  ;
}
