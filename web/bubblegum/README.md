# //web/bubblegum

`bubblegum` is a CGI programming library for the Nix expression language.
It provides a few helpers to make writing CGI scripts which are executable
using [//nix/nint](../../nix/nint/README.md) convenient.

An example nix.cgi script looks like this (don't worry about the shebang
too much, you can use `web.bubblegum.writeCGI` to set this up without
thinking twice):

```nix
#!/usr/bin/env nint --arg depot '(import /path/to/depot {})'
{ depot, ... }:

let
  inherit (depot.web.bubblegum)
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

As you can see, the core component of `bubblegum` is the `respond`
function which takes three arguments:

* The response status as the textual representation which is also
  returned to the client in the HTTP protocol, e. g. `"OK"`,
  `"Not Found"`, `"Bad Request"`, …

* An attribute set mapping header names to header values to be sent.

* The response body as a string.

Additionally it exposes a few helpers for working with the CGI
environment like `pathInfo` which is a wrapper around
`builtins.getEnv "PATH_INFO"`. The documentation for all exposed
helpers is inlined in [default.nix](./default.nix) (you should be
able to use `nixdoc` to render it).

For deployment purposes it is recommended to use `writeCGI` which
takes a nix CGI script in the form of a derivation, path or string
and builds an executable nix CGI script which has the correct shebang
set and is automatically passed a version of depot from the nix store,
so the script has access to the `bubblegum` library.

For example nix CGI scripts and a working deployment using `thttpd`
see the [examples directory](./examples). You can also start a local
server running the examples like this:

```
$ nix-build -A web.bubblegum.examples && ./result
# navigate to http://localhost:9000
```
