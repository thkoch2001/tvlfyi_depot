# panettone

A simple issue tracker.

## development

panettone is developed by running the service itself and a postgres
database locally and forwarding everything else needed from whitby.
As a prerequisite, make sure you can connect to whitby via SSH.
Then the following things need to be started (in parallel):

1. `ssh -NL 3899:localhost:389 -L 4238:localhost:4238 whitby.tvl.fyi`:
   This forwards LDAP and the cheddar markdown conversion service.
2. `nc -l 4722 -k` (with OpenBSD netcat): This simulates irccat,
   since we want to avoid sending notifications to `#tvl` for
   operations on test data.
3. `docker-compose up -f web/panettone/docker-compose.yml`:
   This starts a local PostgreSQL database on the right port.
4. Finally, launch panettone, either from sly/SLIME or via
   `nix-build -A web.panettone && ./result/bin/panettone`.
   Since we are using a non-standard port for LDAP, be sure
   to export `LDAP_PORT=3899` beforehands.

Now, navigate to `http://localhost:6161` to interact with
the local instance.

When invoking panettone interactively, you can set
`(setq hunchentoot:*catch-errors-p* nil)` beforehands,
so you'll enter a debugger on any failure.
