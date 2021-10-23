install-cli-tools:
	nix-env -f "${BRIEFCASE}" -iA shared.cliTools

install-configs:
	nix-build -A configs.install && \
	./result && \
	rm ./result

uninstall-configs:
	nix-build -A configs.uninstall && \
	./result && \
	rm ./result

list-broken-links:
	nix-build -A tools.symlinkManager && \
	./result/bin/symlink-mgr -audit && \
	rm ./result
