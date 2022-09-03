{ stdenv, fetchzip, makeWrapper }:

stdenv.mkDerivation {
  pname = "ghidra";
  version = "9.1-BETA";

  src = fetchzip {
    url = "https://ghidra-sre.org/ghidra_9.1-BETA_DEV_20190923.zip";
    sha256 = "16xq62vkgiq4l3cy30psghl4g4g3gkmg2yb3sxcaz6nj51ahm51b";
  };

  nativeBuildInputs = [ makeWrapper ];

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp -r $src/* $out

    makeWrapper "$out/ghidraRun" "$out/bin/ghidra" \
      --set "_JAVA_AWT_WM_NONREPARENTING=1"
  '';

  meta = {
    description = "Open-source NSA reverse engineering tool";
    homepage = https://ghidra-sre.org/;
    mantainers = [];
    platforms = stdenv.lib.platforms.all;
  };
}
