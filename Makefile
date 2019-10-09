install:
	source "${DOTFILES}/configs/install"

uninstall:
	source "${DOTFILES}/configs/uninstall"

list-broken-links:
	find "${HOME}" -maxdepth 1 -xtype l && \
	find "${HOME}/.config" -maxdepth 1 -xtype l
