# nix.cgi

nix.cgi is a CGI programming library for the Nix expression language.
It allows writing CGI scripts in pure nix in the form of nix expressions
which conform to a certain calling convention.

An example nix.cgi script looks like this:

```nix
#!/usr/bin/env ncla-run --arg depot '(import /path/to/depot {})'
{ depot, ... }:

let
  inherit (depot.web.nix-cgi)
    respond
    ;
in

respond "OK" {
  "Content-type" = "text/html";
  # further headers…
} ''
  <!doctype html>
  <html>
    <head>
      <meta charset="utf-8">
      <title>hello world</title>
    </head>
    <body>
      hello world!
    </body>
  </html>
''
```

As you can see, the core component of nix.cgi is the `respond`
function which takes three arguments:

* The response status as the textual representation which is also
  returned to the client in the HTTP protocol, e. g. `"OK"`,
  `"Not Found"`, `"Bad Request"`, …

* An attribute set mapping header names to header values to be sent.

* The response body as a string.

Additionally it exposes a few helpers for working with the CGI
environment like `pathInfo` which is a wrapper around
`builtins.getEnv "PATH_INFO"` which ensures that the “root”
always returns `/` for both `PATH_INFO=""` and `PATH_INFO="/"`.

For deployment purposes it is recommended to use `writeCGI` which
takes a nix CGI script in the form of a derivation, path or string
and builds an executable nix CGI script which has the correct shebang
set and is automatically passed a version of depot from the nix store,
so the script can import this library.

For example nix CGI scripts and a working deployment using `thttpd`
see the [examples directory](./examples). You can also run the examples
like this:

```
$ nix-build -A users.sterni.nix.cgi.examples && ./result
# navigate to http://localhost:9000
```
