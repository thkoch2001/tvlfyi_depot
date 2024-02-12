# Brooklyn-Based Board Gaming signup sheet

This directory contains a small web application that acts as a signup
sheet and attendee tracking system for [my local board gaming
meetup](https://www.meetup.com/brooklyn-based-board-gaming/).

## Development

### Installing dependencies

#### With Nix + Docker ("blessed way")

Prerequisites:

-   [Nix](https://nixos.org/)
-   [lorri](https://github.com/nix-community/lorri)
-   [Docker](https://www.docker.com/)

From this directory in a full checkout of depot, run the following
commands to install all development dependencies:

``` shell-session
$ pwd
/path/to/depot/users/aspen/bbbg
$ direnv allow
$ lorri watch --once # Wait for a single nix shell build
```

Then, to run a docker container with the development database:

``` shell-session
$ pwd
/path/to/depot/users/aspen/bbbg
$ arion up -d
```

#### Choose-your-own-adventure

Note that the **authoritative** source for dev dependencies is the `shell.nix`
file in this directory - those may diverge from what's written here; if so
follow those versions rather than these.

-   Install the [clojure command-line
    tools](https://clojure.org/guides/getting_started), with openjdk 11
-   Install and run a postgresql 12 database, with:
    -   A user with superuser priveleges, the username `bbbg` and the
        password `password`
    -   A database called `bbbg` owned by that user.
-   Export the following environment variables in a context visible by
    whatever method you use to run the application:
    -   `PGHOST=localhost`
    -   `PGUSER=bbbg`
    -   `PGDATABASE=bbbg`
    -   `PGPASSWORD=bbbg`

### Running the application

Before running the app, you'll need an oauth2 client-id and client secret for a
Discord app. The application can either load those from a
[pass](https://www.passwordstore.org/) password store, or read them from
plaintext files in a directory. In either case, they should be accessible at the
paths `bbbg/discord-client-id` and `bbbg/discord-client-secret` respectively.

#### From the command line

``` shell-session
$ clj -A:dev
Clojure 1.11.0-alpha3
user=> (require 'bbbg.core)
nil
user=> ;; Optionally, if you're using a directory with plaintext files for the discord client ID and client secret:
user=> (bbbg.util.dev-secrets/set-backend! [:dir "/path/to/that/directory"])
user=> (bbbg.core/run-dev)
##<SystemMap>
user=> (bbbg.db/migrate! (:db bbbg.core/system))
11:57:26.536 [main] INFO  migratus.core - Starting migrations {  }
11:57:26.538 [main] INFO  com.zaxxer.hikari.HikariDataSource - HikariPool-1 - Starting... {  }
11:57:26.883 [main] INFO  com.zaxxer.hikari.pool.HikariPool - HikariPool-1 - Added connection com.impossibl.postgres.jdbc.PGDirectConnection@3cae770e {  }
11:57:26.884 [main] INFO  com.zaxxer.hikari.HikariDataSource - HikariPool-1 - Start completed. {  }
11:57:26.923 [main] INFO  migratus.core - Ending migrations {  }
nil
```

This will run a web server for the application listening at
<http://localhost:8888>

#### In Emacs, with [CIDER](https://docs.cider.mx/cider/index.html) + [direnv](https://github.com/wbolster/emacs-direnv)

Open `//users/aspen/bbbg/src/bbbg/core.clj` in a buffer, then follow the
instructions at the end of the file

## Deployment

### With nix+terraform

Deployment configuration is located in the `tf.nix` file, which is
currently tightly coupled to my own infrastructure and AWS account but
could hypothetically be adjusted to be general-purpose.

To deploy a new version of the application, after following "installing
dependencies" above, run the following command in a context with ec2
credentials available:

``` shell-session
$ terraform apply
```

The current deploy configuration includes:

-   An ec2 instance running nixos, with a postgresql database and the
    bbbg application running as a service, behind nginx with an
    auto-renewing letsencrypt cert
-   The DNS A record for `bbbg.gws.fyi` pointing at that ec2 instance,
    in the cloudflare zone for `gws.fyi`

### Otherwise

¯\\\_(ツ)_/¯

You'll need:

-   An uberjar for bbbg; the canonical way of building that is `nix-build
    /path/to/depot -A users.aspen.bbbg.server-jar` but I\'m not sure how that
    works outside of nix
-   A postgresql database
-   Environment variables telling the app how to connect to that
    database. See `config.systemd.services.bbbg-server.environment` in
    `module.nix` for which env vars are currently being exported by the
    NixOS module that runs the production version of the app
