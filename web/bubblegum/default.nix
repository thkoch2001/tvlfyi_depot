{ depot, lib, pkgs, ... }:

let

  inherit (depot.nix)
    runExecline
    getBins
    utils
    sparseTree
    nint
    ;

  minimalDepot = sparseTree depot.path.origSrc [
    # general depot things
    "default.nix"
    "nix/readTree"
    # nixpkgs for lib and packages
    "third_party/nixpkgs"
    "third_party/overlays"
    # bubblegum and its dependencies
    "web/bubblegum"
    "nix/runExecline"
    "nix/utils"
    "nix/sparseTree"
    # tvix docs for svg demo
    "tvix/docs"
    # for blog.nix
    "users/sterni/nix"
  ];

  statusCodes = {
    # 1xx
    "Continue" = 100;
    "Switching Protocols" = 101;
    "Processing" = 102;
    "Early Hints" = 103;
    # 2xx
    "OK" = 200;
    "Created" = 201;
    "Accepted" = 202;
    "Non-Authoritative Information" = 203;
    "No Content" = 204;
    "Reset Content" = 205;
    "Partial Content" = 206;
    "Multi Status" = 207;
    "Already Reported" = 208;
    "IM Used" = 226;
    # 3xx
    "Multiple Choices" = 300;
    "Moved Permanently" = 301;
    "Found" = 302;
    "See Other" = 303;
    "Not Modified" = 304;
    "Use Proxy" = 305;
    "Switch Proxy" = 306;
    "Temporary Redirect" = 307;
    "Permanent Redirect" = 308;
    # 4xx
    "Bad Request" = 400;
    "Unauthorized" = 401;
    "Payment Required" = 402;
    "Forbidden" = 403;
    "Not Found" = 404;
    "Method Not Allowed" = 405;
    "Not Acceptable" = 406;
    "Proxy Authentication Required" = 407;
    "Request Timeout" = 408;
    "Conflict" = 409;
    "Gone" = 410;
    "Length Required" = 411;
    "Precondition Failed" = 412;
    "Payload Too Large" = 413;
    "URI Too Long" = 414;
    "Unsupported Media Type" = 415;
    "Range Not Satisfiable" = 416;
    "Expectation Failed" = 417;
    "I'm a teapot" = 418;
    "Misdirected Request" = 421;
    "Unprocessable Entity" = 422;
    "Locked" = 423;
    "Failed Dependency" = 424;
    "Too Early" = 425;
    "Upgrade Required" = 426;
    "Precondition Required" = 428;
    "Too Many Requests" = 429;
    "Request Header Fields Too Large" = 431;
    "Unavailable For Legal Reasons" = 451;
    # 5xx
    "Internal Server Error" = 500;
    "Not Implemented" = 501;
    "Bad Gateway" = 502;
    "Service Unavailable" = 503;
    "Gateway Timeout" = 504;
    "HTTP Version Not Supported" = 505;
    "Variant Also Negotiates" = 506;
    "Insufficient Storage" = 507;
    "Loop Detected" = 508;
    "Not Extended" = 510;
    "Network Authentication Required" = 511;
  };

  /* Generate a CGI response. Takes three arguments:

    1. Status of the response as a string which is
    the descriptive name in the protocol, e. g.
    `"OK"`, `"Not Found"` etc.
    2. Attribute set describing extra headers to
    send, keys and values should both be strings.
    3. Response content as a string.

    See the [README](./README.md) for an example.

    Type: either int string -> attrs string -> string -> string
  */
  respond =
    # response status as an integer (status code) or its
    # textual representation in the HTTP protocol.
    # See `statusCodes` for a list of valid options.
    statusArg:
    # headers as an attribute set of strings
    headers:
    # response body as a string
    bodyArg:
    let
      status =
        if builtins.isInt statusArg
        then {
          code = statusArg;
          line = lib.findFirst
            (line: statusCodes."${line}" == statusArg)
            null
            (builtins.attrNames statusCodes);
        } else if builtins.isString statusArg then {
          code = statusCodes."${statusArg}" or null;
          line = statusArg;
        } else {
          code = null;
          line = null;
        };
      renderedHeaders = lib.concatStrings
        (lib.mapAttrsToList (n: v: "${n}: ${toString v}\r\n") headers);
      internalError = msg: respond 500
        {
          Content-type = "text/plain";
        } "bubblegum error: ${msg}";
      body = builtins.tryEval bodyArg;
    in
    if status.code == null || status.line == null
    then internalError "Invalid status ${lib.generators.toPretty {} statusArg}."
    else if !body.success
    then internalError "Unknown evaluation error in user code"
    else
      lib.concatStrings [
        "Status: ${toString status.code} ${status.line}\r\n"
        renderedHeaders
        "\r\n"
        body.value
      ];

  /* Returns the value of the `SCRIPT_NAME` environment
    variable used by CGI.
  */
  scriptName = builtins.getEnv "SCRIPT_NAME";

  /* Returns the value of the `PATH_INFO` environment
    variable used by CGI. All cases that could be
    considered as the CGI script's root (i. e.
    `PATH_INFO` is empty or `/`) is mapped to `"/"`
    for convenience.
  */
  pathInfo =
    let
      p = builtins.getEnv "PATH_INFO";
    in
    if builtins.stringLength p == 0
    then "/"
    else p;

  /* Helper function which converts a path from the
    root of the CGI script (i. e. something which
    could be the content of `PATH_INFO`) to an
    absolute path from the web root by also
    utilizing `scriptName`.

    Type: string -> string
  */
  absolutePath = path:
    if builtins.substring 0 1 path == "/"
    then "${scriptName}${path}"
    else "${scriptName}/${path}";

  bins = getBins pkgs.coreutils [ "env" "tee" "cat" "printf" "chmod" ]
    // getBins nint [ "nint" ];

  /* Type: args -> either path derivation string -> derivation
  */
  writeCGI =
    {
      # if given sets the `PATH` to search for `nix-instantiate`
      # Useful when using for example thttpd which unsets `PATH`
      # in the CGI environment.
      binPath ? ""
      # name of the resulting derivation. Defaults to `baseNameOf`
      # the input path or name of the input derivation.
      # Must be given if the input is a string.
    , name ? null
    , ...
    }@args:
    input:
    let
      drvName =
        if builtins.isString input || args ? name
        then args.name
        else utils.storePathName input;
      script =
        if builtins.isPath input || lib.isDerivation input
        then input
        else if builtins.isString input
        then pkgs.writeText "${drvName}-source" input
        else builtins.throw "Unsupported input: ${lib.generators.toPretty {} input}";
      shebang = lib.concatStringsSep " " ([
        "#!${bins.env}"
        # use the slightly cursed /usr/bin/env -S which allows us
        # to pass any number of arguments to our interpreter
        # instead of maximum one using plain shebang which considers
        # everything after the first space as the second argument.
        "-S"
      ] ++ lib.optionals (builtins.stringLength binPath > 0) [
        "PATH=${binPath}"
      ] ++ [
        "${bins.nint}"
        # always pass depot so scripts can use this library
        "--arg depot '(import ${minimalDepot} {})'"
      ]);
    in
    runExecline.local drvName { } [
      "importas"
      "out"
      "out"
      "pipeline"
      [
        "foreground"
        [
          "if"
          [ bins.printf "%s\n" shebang ]
        ]
        "if"
        [ bins.cat script ]
      ]
      "if"
      [ bins.tee "$out" ]
      "if"
      [ bins.chmod "+x" "$out" ]
      "exit"
      "0"
    ];

in
{
  inherit
    respond
    pathInfo
    scriptName
    absolutePath
    writeCGI
    ;
}
