self: super:

let

  cfgPath = ../config/autorandr;

  xdgConfigHome' = super.symlinkJoin {
    name = "xdg-config-home";
    paths = [ "${cfgPath}" ];
    postBuild = ''
      ln -s $out $out/autorandr
    '';
  };

  autorandr' = super.autorandr.overrideAttrs (oldAttrs: {
    installPhase = ''
      runHook preInstall

      make install TARGETS=autorandr PREFIX=$out

      runHook postInstall
    '';
  });

in {
  autorandr =
    super.symlinkJoin {
      name = "autorandr";
      paths = [ autorandr' ];
      nativeBuildInputs = [ super.makeWrapper ];
      postBuild = ''
        cp --remove-destination "$(readlink $out/bin/autorandr)" $out/bin/autorandr

        substituteInPlace $out/bin/autorandr \
          --replace 'autorandr_binary = os.path.abspath(argv[0])' \
                    "autorandr_binary = os.path.abspath('$out/bin/autorandr')"

        wrapProgram $out/bin/autorandr \
          --set XDG_CONFIG_HOME ${xdgConfigHome'}
      '';
    };
}
