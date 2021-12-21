{ pkgs, ... }:
let
  xmobarOpts = ''
    ${pkgs.blueman}/bin/blueman-applet &
    ${pkgs.nitrogen}/bin/nitrogen --restore &
    ${pkgs.trayer}/bin/trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34  --height 22 &
    ${pkgs.xfce.xfce4-notifyd}/bin/xfce4-notifyd &
    ${pkgs.gnome.pomodoro}/bin/gnome-pomodoro &
    ${pkgs.volumeicon}/bin/volumeicon &
    '';
  extras = ''
    ${pkgs.dropbox}/bin/dropbox start
    '';

in {
   xsession = {
    enable = true;

    initExtra = extras + xmobarOpts;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
      ];
      config = ./xmonad.hs;
    };
  };
}
