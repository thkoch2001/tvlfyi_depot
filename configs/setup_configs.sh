#!/usr/bin/env bash


# .zsh_profile
if [ -f "$HOME"/.zsh_profile ] && [ ! -L "$HOME"/.zsh_profile ]; then
    # backup .zsh_profile
    echo -n "Backing up .zsh_profile ... " && \
    mv "$HOME"/.zsh_profile "$HOME"/.zsh_profile.bak && \
    echo "Done."
fi

if [ -L "$HOME"/.zsh_profile ]; then
    # TODO: make sure that .zsh_profile is symlinked to the correct location.
    echo ".zsh_profile is already symlinked."
else
    # create symlink to pc_settings .zsh_profile
    echo -n "Symlinking to pc_settings/configs/.zsh_profile ... " && \
    ln -s "$HOME"/pc_settings/configs/.zsh_profile "$HOME"/.zsh_profile && \
    echo "Done."
fi


# .tmux.conf
if [ -f "$HOME"/.tmux.conf ] && [ ! -L "$HOME"/.tmux.conf ]; then
    echo -n "Backing up .tmux.conf ... " && \
    mv "$HOME"/.tmux.conf "$HOME"/.tmux.conf.bak && \
    echo "Done."
fi

if [ -L "$HOME"/.tmux.conf ]; then
    # TODO: make sure that .tmux.conf is symlinked to the correct location.
    echo ".tmux.conf is already symlinked."
else
    # create symlink to pc_settings .tmux.conf
    echo -n "Symlinking to pc_settings/configs/.tmux.conf ... " && \
    ln -s "$HOME"/pc_settings/configs/.tmux.conf "$HOME"/.tmux.conf && \
    echo "Done."
fi


# .ctags
if [ -f "$HOME"/.ctags ] && [ ! -L "$HOME"/.ctags ]; then
    # backup .ctags
    echo -n "Backing up .ctags ... " && \
    mv "$HOME"/.ctags "$HOME"/.ctags.bak && \
    echo "Done."
fi

if [ -L "$HOME"/.ctags ]; then
    # TODO: make sure that .ctags is symlinked to the correct location.
    echo ".ctags is already symlinked."
else
    # create symlink to pc_settings .ctags
    echo -n "Symlinking to pc_settings/configs/.ctags ... " && \
    ln -s "$HOME"/pc_settings/configs/.ctags "$HOME"/.ctags && \
    echo "Done."
fi

