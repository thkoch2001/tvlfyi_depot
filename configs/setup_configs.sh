#!/usr/bin/env bash


pc_settings_path="$HOME/pc_settings"


config_files=( ".zsh_profile" ".tmux.conf" ".ctags" )


for i in {1..3}; do
    cf="${config_files[i]}"
    if [ -f "$HOME/$cf" ] && [ ! -L "$HOME/$cf" ]; then
        echo -n "Backing up $cf ... " && \
        mv "$HOME/$cf" "$HOME/$cf.bak" && \
        echo "Done."
    fi

    if [ -L "$HOME/$cf" ]; then
        if [ $(readlink "$HOME/$cf") = "$pc_settings_path/configs/$cf" ]; then
            echo "\"$cf\" is already properly symlinked to \"$pc_settings_path/configs\"."
        else
            echo "\"$cf\" is symlinked but not to the proper location."
        fi
    else
        echo -n "Symlinking to $pc_settings_path/configs/$cf ... " && \
        ln -s "$pc_settings_path/configs/$cf" "$HOME/$cf" && \
        echo "Done."
    fi
done

