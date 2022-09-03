self: super:

let

  dunstrc = ../config/dunst/dunstrc;

in {
  dunst = super.dunst.overrideAttrs (oldAttrs: {
    postInstall = ''
      wrapProgram $out/bin/dunst \
        --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE" \
        --add-flags "-config '${dunstrc}'"
    '';
  });
}
