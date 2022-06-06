{ depot, pkgs, lib, config, ... }:

# TODO(sterni): automatically sync repositories with upstream if needed
let
  virtualHost = "code.sterni.lv";

  repos = {
    spacecookie = {
      description = "gopher server (and library for Haskell)";
    };
    gopher-proxy = {
      description = "Gopher over HTTP proxy";
    };
    emoji-generic = {
      description = "generic emoji library for Haskell (wip)";
    };
    grav2ty = {
      description = "“realistic” 2d space game";
    };
    likely-music = {
      description = "experimental application for probabilistic music composition";
    };
    logbook = {
      description = "file format for keeping a personal log";
    };
    sternenblog = {
      description = "file based cgi blog software";
    };
    haskell-dot-time = {
      description = "UTC-centric time library for haskell with dot time support";
      defaultBranch = "main";
    };
    buchstabensuppe = {
      description = "toy font rendering for low pixelcount, high contrast displays";
      defaultBranch = "main";
    };
  };

  cgitRepoEntry = name: repo:
    let
      repoName = repos.name or name;
      path = repo.path or "${repoName}.git";
    in
    lib.concatStringsSep "\n" (
      [
        "repo.url=${repoName}"
        "repo.path=/srv/git/${path}"
      ]
      ++ lib.optional (repo ? description) "repo.desc=${repo.description}"
      ++ lib.optional (repo ? defaultBranch) "repo.defbranch=${repo.defaultBranch}"
    );

  cgitHead = pkgs.writeText "cgit-head.html" ''
    <style>
    #summary {
      max-width: 80em;
    }

    #summary * {
      max-width: 100%;
    }
    </style>
  '';

  cgitConfig = pkgs.writeText "cgitrc" ''
    virtual-root=/

    enable-http-clone=1
    clone-url=https://${virtualHost}/$CGIT_REPO_URL

    enable-blame=1
    enable-log-filecount=1
    enable-log-linecount=1
    enable-index-owner=0
    enable-blame=1
    enable-commit-graph=1

    root-title=code
    root-desc=sterni's git repositories
    css=/cgit.css
    head-include=${cgitHead}

    mimetype-file=${pkgs.mime-types}/etc/mime.types

    about-filter=${depot.tools.cheddar.about-filter}/bin/cheddar-about
    source-filter=${depot.tools.cheddar}/bin/cheddar
    readme=:README.md
    readme=:readme.md

    ${builtins.concatStringsSep "\n\n" (lib.mapAttrsToList cgitRepoEntry repos)}
  '';
in

{
  imports = [
    ./nginx.nix
    ./fcgiwrap.nix
  ];

  config = {
    services.nginx.virtualHosts."${virtualHost}" = {
      enableACME = true;
      forceSSL = true;
      root = "${pkgs.cgit-pink}/cgit/";
      extraConfig = ''
        try_files $uri @cgit;

        location @cgit {
          include ${pkgs.nginx}/conf/fastcgi_params;
          fastcgi_param    SCRIPT_FILENAME ${pkgs.cgit-pink}/cgit/cgit.cgi;
          fastcgi_param    PATH_INFO       $uri;
          fastcgi_param    QUERY_STRING    $args;
          fastcgi_param    HTTP_HOST       $server_name;
          fastcgi_param    CGIT_CONFIG     ${cgitConfig};
          fastcgi_pass     unix:${toString config.services.fcgiwrap.socketAddress};
        }
      '';
    };
  };
}
