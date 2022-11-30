{ depot, pkgs, lib, config, ... }:

let
  virtualHost = "code.sterni.lv";

  repoSections = [
    {
      section = "active";
      repos = {
        spacecookie = {
          description = "gopher server (and library for Haskell)";
          upstream = "https://github.com/sternenseemann/spacecookie.git";
        };
      };
    }
    {
      section = "poc";
      repos = {
        emoji-generic = {
          description = "generic emoji library for Haskell";
          upstream = "https://github.com/sternenseemann/emoji-generic.git";
        };
        grav2ty = {
          description = "“realistic” 2d space game";
          upstream = "https://github.com/sternenseemann/grav2ty.git";
        };
        haskell-dot-time = {
          description = "UTC-centric time library for haskell with dot time support";
          defaultBranch = "main";
        };
        buchstabensuppe = {
          description = "toy font rendering for low pixelcount, high contrast displays";
          defaultBranch = "main";
          upstream = "https://github.com/sternenseemann/buchstabensuppe.git";
        };
      };
    }
    {
      section = "archive";
      repos = {
        gopher-proxy = {
          description = "Gopher over HTTP proxy";
          upstream = "https://github.com/sternenseemann/gopher-proxy.git";
        };
        likely-music = {
          description = "experimental application for probabilistic music composition";
          upstream = "https://github.com/sternenseemann/likely-music.git";
        };
        logbook = {
          description = "file format for keeping a personal log";
          upstream = "https://github.com/sternenseemann/logbook.git";
        };
        sternenblog = {
          description = "file based cgi blog software";
          upstream = "https://github.com/sternenseemann/sternenblog.git";
        };
      };
    }
  ];

  repoPath = name: repo: repo.path or "/srv/git/${name}.git";

  cgitRepoEntry = name: repo:
    lib.concatStringsSep "\n" (
      [
        "repo.url=${name}"
        "repo.path=${repoPath name repo}"
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

    root-title=code.sterni.lv
    css=/cgit.css
    head-include=${cgitHead}

    mimetype-file=${pkgs.mime-types}/etc/mime.types

    about-filter=${depot.tools.cheddar.about-filter}/bin/cheddar-about
    source-filter=${depot.tools.cheddar}/bin/cheddar
    readme=:README.md
    readme=:readme.md

    section-sort=0
    ${
      lib.concatMapStringsSep "\n" (section:
        ''
          section=${section.section}
        ''
        + builtins.concatStringsSep "\n\n" (lib.mapAttrsToList cgitRepoEntry section.repos)
      ) repoSections
    }
  '';

  /* Merge a list of attrs, but fail when the same attribute occurs twice.

     Type: [ attrs ] -> attrs
  */
  mergeManyDistinctAttrs = lib.foldAttrs
    (
      val: nul:
        if nul == null then val else throw "Every attribute name may occur only once"
    )
    null;

  flatRepos = mergeManyDistinctAttrs
    (builtins.map (section: section.repos) repoSections);

  reposToMirror = lib.filterAttrs (_: repo: repo ? upstream) flatRepos;

  # User and group name used for running the mirror scripts
  mirroredReposOwner = "git";
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

    users = {
      users.${mirroredReposOwner} = {
        group = mirroredReposOwner;
        isSystemUser = true;
      };

      groups.${mirroredReposOwner} = { };
    };


    systemd.timers = lib.mapAttrs'
      (
        name: repo:
          {
            name = "mirror-${name}";
            value = {
              description = "regularly update mirror git repository ${name}";
              wantedBy = [ "timers.target" ];
              enable = true;
              timerConfig = {
                # Fire every 6h and distribute the workload over next 6h randomly
                OnCalendar = "*-*-* 00/6:00:00";
                AccuracySec = "6h";
                RandomizedDelaySec = "6h";
                Persistent = true;
              };
            };
          }
      )
      reposToMirror;

    systemd.services = lib.mapAttrs'
      (
        name: repo:
          {
            name = "mirror-${name}";
            value = {
              description = "mirror git repository ${name}";
              after = [ "network.target" ];
              script =
                let
                  path = repoPath name repo;
                in
                ''
                  set -euo pipefail

                  export PATH="${lib.makeBinPath [ pkgs.coreutils pkgs.git ]}"

                  if test ! -d "${path}"; then
                    mkdir -p "$(dirname "${path}")"
                    git clone --mirror "${repo.upstream}" "${path}"
                    exit 0
                  fi

                  cd "${path}"

                  git fetch "${repo.upstream}" '+refs/*:refs/*' --prune
                '';

              serviceConfig = {
                Type = "oneshot";
                User = mirroredReposOwner;
                Group = mirroredReposOwner;
              };
            };
          }
      )
      reposToMirror;
  };
}
