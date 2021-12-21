{ config, lib, pkgs, ... }:

let
  fishConfig = ''
    function vterm_printf;
        if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
            # tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
        else if string match -q -- "screen*" "$TERM"
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$argv"
        else
            printf "\e]%s\e\\" "$argv"
        end
    end
'';

in
{
  programs.fish = {

    enable = true;
    shellAliases = {
      ff = "emacsclient";
      vim = "emacsclient";
      nixconf = "emacsclient -c -a 'emacs' ~/nix-config/configuration.nix";
      hedit = "emacsclient -c -a 'emacs' ~/nix-config/nixpkgs/home.nix";
      hswitch = "sh ~/nix-config/apply-home.sh";
      nixreb="sh ~/nix-config/apply-system.sh";
	    ls = "exa -lah --group-directories-first";
  };
    shellInit = fishConfig;
};
}
