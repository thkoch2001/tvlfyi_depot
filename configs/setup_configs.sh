#!/usr/bin/env bash


pc_settings_path="${HOME}/pc_settings"


config_files=( \
  ".zsh_profile" \
  ".tmux.conf" \
  ".ctags" \
  ".vimrc" \
  ".emacs" \
)


for i in {1..5}; do
    cf="${config_files[i]}"
    echo "\"${cf}\": "

    if [ -f "${HOME}/${cf}" ] && [ ! -L "${HOME}/${cf}" ]; then
        echo -n "Backing up ${cf} ... " && \
        mv "${HOME}/${cf}" "${HOME}/${cf}.bak" && \
        echo "Done."
    fi

    if [ -L "${HOME}/${cf}" ]; then
        if [ $(readlink "${HOME}/${cf}") = "${pc_settings_path}/configs/${cf}" ]; then
            echo "Already properly symlinked to \"${pc_settings_path}/configs\"."
        else
            echo "Already symlinked but NOT to the proper location. Aborting..."
        fi
    else
        echo -n "Symlinking to ${pc_settings_path}/configs/${cf} ... " && \
        ln -s "${pc_settings_path}/configs/${cf}" "${HOME}/${cf}" && \
        echo "Done."
    fi
    echo ""
done
