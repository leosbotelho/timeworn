self: super:

{
  slim = super.slim.overrideAttrs (oldAttrs: {
    postInstall = ''
      echo 'tty_lock 1' > $out/etc/slimlock.conf
    '';
  });
}
