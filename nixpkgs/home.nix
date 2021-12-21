{ config, pkgs, files,nix-doom-emacs,  ... }:


let
    defaultPackages = with pkgs; [
    alacritty
    python39
    cmake
    htop
    tldr
    git-crypt
    gnupg
    ripgrep
    git 
    wally-cli
    youtube-dl
    spago

    exa                  # a better `ls`
    fd                   # "find" for files
  ];


in
  {

    imports = (import ./programs);

  home = {
     packages = defaultPackages;
     file = {
       ideavimrc.source = ./dotfiles/ideavimrc;
       ideavimrc.target = "/Users/pbk/.ideavimrc";
     };

     username = "pbk";
     homeDirectory = "/home/pbk";
     stateVersion = "21.03";
  };



  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
}
