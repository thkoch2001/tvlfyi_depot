#!/usr/bin/env bash

dotfiles_path="${HOME}/dotfiles"
configs_dir="${dotfiles_path}/configs"
shared_configs="${configs_dir}/shared"

if [[  $(uname) == 'Darwin' ]]; then
    os_specific_configs="${configs_dir}/os_x"
elif [[ $(uname) == 'Linux' ]]; then
    os_specific_configs="${configs_dir}/linux"
fi


function symlink_configs () {
    configs_dir=$1

    for cf in $(find $configs_dir -type f -name ".*"); do
        filename=$(grep -o "[^\/]+$" <<<$cf)
        echo "$filename: "

        if [ -f "${HOME}/${filename}" ] && [ ! -L "${HOME}/${filename}" ]; then
            echo -n "Backing up ${filename}... " && \
            mv "${HOME}/${filename}" "${HOME}/${filename}.bak" && \
            echo "Done."
        fi

        if [ -L "${HOME}/${filename}" ]; then
            if [ $(readlink "${HOME}/${filename}") = $cf ]; then
                echo "Already properly symlinked to ${configs_dir}."
            else
                echo "Already symlinked but NOT to the proper location. Aborting..."
            fi
        else
            echo -n "Symlinking to ${filename}... " && \
            ln -s $cf "${HOME}/${filename}" && \
            echo "Done."
        fi
        echo ""
    done
}


# handle shared configs
symlink_configs $shared_configs

# handle os-specific configs
symlink_configs $os_specific_configs
