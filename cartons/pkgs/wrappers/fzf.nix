{ pkgs, ... }:

let

  fzf =
    pkgs.symlinkJoin {
      name = "fzf";
      paths = [ pkgs.fzf ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/fzf \
          --add-flags "--color=fg:0,hl:12,fg+:243,bg+:251,hl+:4,info:0,marker:10,prompt:12"
      '';
    };

in {
  environment.systemPackages = [ fzf ];

  environment.variables = {
    FZF_DEFAULT_COMMAND = "fd . $HOME";
    FZF_CTRL_T_COMMAND = "$FZF_DEFAULT_COMMAND";
    FZF_ALT_C_COMMAND = "fd -t -d . $HOME";
  };
}
