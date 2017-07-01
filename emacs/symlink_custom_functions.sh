#!/usr/bin/env zsh

SCRIPT_DIR="$HOME/pc_settings/emacs"
EMACS_FUNC_DIR="$HOME/.emacs.d"

for source in $(find $SCRIPT_DIR -type f -name '*.el'); do
  filename=$(basename $source)
  target="${EMACS_FUNC_DIR}/${filename}"

  if [ ! -L $target ]; then
    echo -n "Creating symlink for ${filename} ... " && \
    ln -s $source $EMACS_FUNC_DIR && \
    echo "Done."
  else
    echo "${filename} is already properly symlinked."
  fi

done
