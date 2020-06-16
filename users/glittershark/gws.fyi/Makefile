.PHONY: deploy

deploy:
	@$(shell nix-build `git rev-parse --show-toplevel` -A 'users.glittershark."gws.fyi"' --no-out-link)

renew:
	@echo Renewing...
	@certbot renew \
		--manual \
		--preferred-challenges dns \
		--server https://acme-v02.api.letsencrypt.org/directory \
		--agree-tos \
		--work-dir $(shell pwd)/letsencrypt/work \
		--logs-dir $(shell pwd)/letsencrypt/logs \
		--config-dir $(shell pwd)/letsencrypt/config

backup:
	@tarsnap -cf $(shell uname -n)-letsencrypt-$(shell date +%Y-%m-%d_%H-%M-%S) \
		letsencrypt/

open:
	$$BROWSER "http://gws.fyi"
