{ config, pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      ctrlp
      deoplete-nvim
      syntastic
      vim-abolish
      vim-airline
      vim-airline-themes
      vim-bufferline
      vim-closetag
      # vim-colors-solarized
      # solarized
      (pkgs.vimUtils.buildVimPlugin {
        name = "vim-colors-solarized";
        src = pkgs.fetchFromGitHub {
          owner = "glittershark";
          repo = "vim-colors-solarized";
          rev = "4857c3221ec3f2693a45855154cb61a2cefb514d";
          sha256 = "0kqp5w14g7adaiinmixm7z3x4w74lv1lcgbqjbirx760f0wivf9y";
        };
      })
      vim-commentary
      vim-dispatch
      vim-endwise
      vim-repeat
      vim-fugitive
      vim-markdown
      vim-nix
      vim-rhubarb
      vim-sexp
      vim-sexp-mappings-for-regular-people
      vim-sleuth
      vim-startify
      vim-surround
      vim-unimpaired
      vinegar
    ];
    extraConfig = ''
      source ${./vimrc}
    '';
  };
}
