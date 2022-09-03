{ pkgs, ... }:

{
  imports = [
    ./base.nix
    ../../x/minimalist.nix
  ];

  environment.systemPackages = with pkgs; [
    pulseaudio
    pulsemixer
    wireguard
    usbutils
    input-utils
    arandr
    autorandr
    xclip
    maim
    ffmpeg-full
    feh
    mupdf
    cmus
    mpv
    vlc
    youtube-dl
    firefox
    chromium
    buku
    aria2
    rtorrent
    irssi
    mediainfo
    asciinema
    bc
    horst
    speedtest-cli
    odt2txt
    antiword
    unrtf
    djvu2pdf
    caffeine-ng
  ];

  hardware.pulseaudio.enable = true;

  services.redshift = {
    enable = true;
    temperature = {
      day = 5500;
      night = 2700;
    };
  };

  programs.bash.shellAliases = {
    bc = "bc -q";
  };
}
