#!/usr/bin/env bash


if [ -d /Volumes/usb_vim ] && \ # usb has mounted
   [ ! -L "$HOME/.vimrc" ] && \   # .vimrc is a symlink
   [ ! -L "$HOME/.vim" ]; then    # .vim dir is a symlink
  . "/Volumes/usb_vim/vim/vim_point_to_usb.sh"
  . "$HOME/pc_settings/launchd_scripts/notice.sh"
else
  . "$HOME/pc_settings/launchd_scripts/notice_2.sh"
fi


