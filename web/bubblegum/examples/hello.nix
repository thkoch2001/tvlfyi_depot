{ depot, ... }:

let
  inherit (depot.third_party.nixpkgs)
    lib
    ;

  inherit (depot.web.bubblegum)
    pathInfo
    respond
    absolutePath
    ;

  routes = {
    "/" = {
      status = "OK";
      title = "index";
      content = ''
        Hello World!
      '';
    };
    "/clock" = {
      status = "OK";
      title = "clock";
      content = ''
        It is ${toString builtins.currentTime}s since 1970-01-01 00:00 UTC.
      '';
    };
    "/coffee" = {
      status = "I'm a teapot";
      title = "coffee";
      content = ''
        No coffee, I'm afraid
      '';
    };
    "/type-error" = {
      status = 666;
      title = "bad usage";
      content = ''
        Never gonna see this.
      '';
    };
    "/eval-error" = {
      status = "OK";
      title = "evaluation error";
      content = builtins.throw "lol";
    };
  };

  notFound = {
    status = "Not Found";
    title = "404";
    content = ''
      This page doesn't exist.
    '';
  };

  navigation =
    lib.concatStrings (lib.mapAttrsToList
      (p: v: "<li><a href=\"${absolutePath p}\">${v.title}</a></li>")
      routes);

  template = { title, content, ... }: ''
    <!doctype html>
    <html lang="en">
    <head>
      <meta charset="utf-8">
      <title>${title}</title>
      <style>a:link, a:visited { color: blue; }</style>
    </head>
    <body>
      <hgroup>
      <h1><code>//web/bubblegum</code></h1>
      <h2>example app</h2>
      </hgroup>
      <header>
        <nav>
          <ul>${navigation}</ul>
        </nav>
      </header>
      <main>
        <p>${content}</p>
      </main>
    </body>
  '';

  response = routes."${pathInfo}" or notFound;

in
respond response.status
{
  "Content-type" = "text/html";
}
  (template response)
