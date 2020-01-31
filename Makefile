install:
	source "${BRIEFCASE}/configs/install"

uninstall:
	source "${BRIEFCASE}/configs/uninstall"

list-broken-links:
	find "${HOME}" -maxdepth 1 -xtype l && \
	find "${HOME}/.config" -maxdepth 1 -xtype l
