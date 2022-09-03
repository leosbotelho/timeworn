self: super:

let

  cfgPath = ../config/vim;

in {
  neovim = super.neovim.override {
    configure = {
      customRC = ''
        ${builtins.readFile (cfgPath + "/vimrc")}
        ${builtins.readFile (cfgPath + "/color.vim")}
      '';

      vam.pluginDictionaries = [
        { name = "vim-better-whitespace"; }
        { name = "fzfWrapper"; }
        { name = "fzf-vim"; }
        { name = "ranger-vim"; }

        { name = "idris-vim"; }
      ];
    };
  };
}
