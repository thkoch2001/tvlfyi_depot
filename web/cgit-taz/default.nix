# This derivation configures a 'cgit' instance to serve repositories
# from a different source.
{ depot, pkgs, ... }:

let
  inherit (pkgs)
    mime-types
    thttpd
    writeShellScriptBin
    writeText
    ;

  sourceFilter = writeShellScriptBin "cheddar-about" ''
    exec ${depot.tools.cheddar}/bin/cheddar --about-filter $@
  '';
  cgitConfig = writeText "cgitrc" ''
    # Global configuration
    virtual-root=/
    enable-http-clone=0
    readme=:README.md
    about-filter=${sourceFilter}/bin/cheddar-about
    source-filter=${depot.tools.cheddar}/bin/cheddar
    enable-log-filecount=1
    enable-log-linecount=1
    enable-follow-links=1
    enable-blame=1
    mimetype-file=${mime-types}/etc/mime.types
    logo=https://static.tvl.fyi/${depot.web.static.drvHash}/logo-animated.svg

    # Repository configuration
    repo.url=depot
    repo.path=/var/lib/gerrit/git/depot.git/
    repo.desc=monorepo for the virus lounge
    repo.owner=The Virus Lounge
    repo.clone-url=https://code.tvl.fyi/depot.git
  '';

  thttpdConfig = writeText "thttpd.conf" ''
    port=2448
    dir=${depot.third_party.cgit}/cgit
    nochroot
    novhost
    cgipat=**.cgi
  '';

  # Patched version of thttpd that serves cgit.cgi as the index and
  # sets the environment variable for pointing cgit at the correct
  # configuration.
  #
  # Things are done this way because recompilation of thttpd is much
  # faster than cgit and I don't want to wait long when iterating on
  # config.
  thttpdConfigPatch = writeText "thttpd_cgit_conf.patch" ''
    diff --git a/libhttpd.c b/libhttpd.c
    index c6b1622..eef4b73 100644
    --- a/libhttpd.c
    +++ b/libhttpd.c
    @@ -3055,4 +3055,6 @@ make_envp( httpd_conn* hc )

         envn = 0;
    +    // force cgit to load the correct configuration
    +    envp[envn++] = "CGIT_CONFIG=${cgitConfig}";
         envp[envn++] = build_env( "PATH=%s", CGI_PATH );
     #ifdef CGI_LD_LIBRARY_PATH
  '';
  thttpdCgit = thttpd.overrideAttrs(old: {
    patches = [
      ./thttpd_cgi_idx.patch
      thttpdConfigPatch
    ];
  });
in writeShellScriptBin "cgit-launch" ''
  exec ${thttpdCgit}/bin/thttpd -D -C ${thttpdConfig}
''
