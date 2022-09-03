{ lib, config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unclutter-xfixes
    dmenu
    slstatus
    feh
    xterm
    dunst
    notify-desktop
  ];

  services.xserver = {
    enable = true;

    windowManager = {
      dwm = {
        enable = true;
      };

      default = "dwm";
    };

    displayManager = {
      sessionCommands = ''
        unclutter &
        xsetroot -solid '#3f3f3f' -cursor_name left_ptr
        xrandr --dpi 135
        slstatus &

        xrdb "${pkgs.writeText "xrdb.conf" ''
          xterm*termName:        xterm-256color

          *faceName:             xft:Monospace:size=12

          *background:           #d9d9d9
          *foreground:           #262626

          *color0:               #000000
          *color1:               #9e1828
          *color2:               #008800
          *color3:               #968a38
          *color4:               #414171
          *color5:               #963c59
          *color6:               #418179
          *color7:               #bebebe
          *color8:               #666666
          *color9:               #cf6171
          *color10:              #7cbc8c
          *color11:              #fff796
          *color12:              #4186be
          *color13:              #cf9ebe
          *color14:              #71bebe
          *color15:              #ffffff
        ''}"

        xdg-settings set default-web-browser firefox
      '';
    };

    libinput.enable = true;
  };
}
