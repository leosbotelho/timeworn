{ stdenv, fetchurl  }:

let

  version = "1.21.2";
  extid = "uBlock0@raymondhill.net";

in stdenv.mkDerivation {
  pname = "ublock-origin";
  inherit version;

  inherit extid;
  signed = true;

  src = fetchurl {
    url = "https://addons.mozilla.org/firefox/downloads/file/3361355/ublock_origin-${version}-an+fx.xpi";
    sha256 = "0ypdq3z61mrymknl37qlq6379bx9f2fsgbgr0czbhqs9f2vwszkc";
  };

  phases = [ "installPhase" ];

  installPhase = ''
    install -D $src "$out/${extid}.xpi"
    '';

  meta = with stdenv.lib; {
    description = "ublock origin firefox browser addon";
    homepage = https://github.com/gorhill/uBlock;
    maintainers = [];
    license = licenses.gpl3;
    platforms = platforms.all;
  };
}
