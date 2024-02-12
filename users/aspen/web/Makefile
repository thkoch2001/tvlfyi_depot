.PHONY: deploy purge_cf do_deploy renew backup open

deploy: do_deploy purge_cf

purge_cf:
	@$(shell nix-build `git rev-parse --show-toplevel` -A 'users.grfn.web.purge-cf')/bin/purge-cf.sh

do_deploy:
	@$(shell nix-build `git rev-parse --show-toplevel` -A 'users.grfn.web')/bin/deploy.sh


renew:
	@echo Renewing...
	@certbot certonly \
		--manual \
		--domain www.gws.fyi \
		--preferred-challenges dns \
		--server https://acme-v02.api.letsencrypt.org/directory \
		--agree-tos \
		--work-dir $(shell pwd)/letsencrypt/work \
		--logs-dir $(shell pwd)/letsencrypt/logs \
		--config-dir $(shell pwd)/letsencrypt/config
	@echo "Reimporting certificate"
	@aws acm import-certificate \
	    --profile personal \
	    --certificate file://letsencrypt/config/live/www.gws.fyi/cert.pem \
	    --certificate-chain file://letsencrypt/config/live/www.gws.fyi/fullchain.pem \
	    --private-key file://letsencrypt/config/live/www.gws.fyi/privkey.pem \
	    --certificate-arn arn:aws:acm:us-east-1:797089351721:certificate/628e54f3-55f9-49c0-811a-eba516b68e30 \
		--region us-east-1

backup:
	@tarsnap -cf $(shell uname -n)-letsencrypt-$(shell date +%Y-%m-%d_%H-%M-%S) \
		letsencrypt/

open:
	$$BROWSER "https://www.gws.fyi"
