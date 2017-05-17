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


# Fish Shell is a special case
# cf_dir="${HOME}/.configs/fish"
# cf="config.fish"

# if [ -f "${cf_dir}/${cf}" ] && [ ! -L "${cf_dir}/${cf}" ]; then
#     echo -n "Backing up ${cf} ... " && \
#     mv "${cf_dir}/${cf}" "${HOME}/${cf}.bak" && \
#     echo "Done."
# fi

# if [ -L "${cf_dir}/${cf}" ]; then
#     if [ $(readlink "${cf_dir}/${cf}") = "${pc_settings_path}/configs/${cf}" ]; then
#         echo "Already properly symlinked to \"${pc_settings_path}/configs\"."
#     else
#         echo "Already symlinked but NOT to the proper location. Aborting..."
#     fi
# else
#     echo -n "Symlinking to ${pc_settings_path}/configs/${cf} ... " && \
#     ln -s "${pc_settings_path}/configs/${cf}" "${cf_dir}/${cf}" && \
#     echo "Done."
# fi
# echo ""
