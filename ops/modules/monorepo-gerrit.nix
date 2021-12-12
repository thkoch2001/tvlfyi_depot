# Gerrit configuration for the TVL monorepo
{ depot, pkgs, config, lib, ... }:

let
  cfg = config.services.gerrit;

  besadiiWithConfig = name:
    pkgs.writeShellScript "besadii-whitby" ''
      export BESADII_CONFIG=/run/agenix/gerrit-besadii-config
      exec -a ${name} ${depot.ops.besadii}/bin/besadii "$@"
    '';

  gerritHooks = pkgs.runCommandNoCC "gerrit-hooks" { } ''
    mkdir -p $out
    ln -s ${besadiiWithConfig "change-merged"} $out/change-merged
    ln -s ${besadiiWithConfig "patchset-created"} $out/patchset-created
  '';
in {
  services.gerrit = {
    enable = true;
    listenAddress = "[::]:4778"; # 4778 - grrt
    serverId = "4fdfa107-4df9-4596-8e0a-1d2bbdd96e36";
    builtinPlugins = [ "download-commands" "hooks" ];

    plugins = with depot.third_party.gerrit_plugins; [
      owners
      oauth
      depot.ops.gerrit-tvl
    ];

    package = depot.third_party.gerrit;

    jvmHeapLimit = "4g";

    # In some NixOS channel bump, the default version of OpenJDK has
    # changed to one that is incompatible with our current version of
    # Gerrit.
    #
    # TODO(tazjin): Update Gerrit and remove this when possible.
    jvmPackage = pkgs.openjdk11_headless;

    settings = {
      core.packedGitLimit = "100m";
      log.jsonLogging = true;
      log.textLogging = false;
      sshd.advertisedAddress = "code.tvl.fyi:29418";
      hooks.path = "${gerritHooks}";
      cache.web_sessions.maxAge = "3 months";
      plugins.allowRemoteAdmin = false;
      change.enableAttentionSet = true;
      change.enableAssignee = false;

      # Configures gerrit for being reverse-proxied by nginx as per
      # https://gerrit-review.googlesource.com/Documentation/config-reverseproxy.html
      gerrit = {
        canonicalWebUrl = "https://cl.tvl.fyi";
        docUrl = "/Documentation";
      };

      httpd.listenUrl = "proxy-https://${cfg.listenAddress}";

      download.command = [ "checkout" "cherry_pick" "format_patch" "pull" ];

      # Configure for cgit.
      gitweb = {
        type = "custom";
        url = "https://code.tvl.fyi";
        project = "/";
        revision = "/commit/?id=\${commit}";
        branch = "/log/?h=\${branch}";
        tag = "/tag/?h=\${tag}";
        roottree = "/tree/?h=\${commit}";
        file = "/tree/\${file}?h=\${commit}";
        filehistory = "/log/\${file}?h=\${branch}";
        linkname = "cgit";
      };

      # Auto-link panettone bug links
      commentlink.panettone = {
        match = "b/(\\\\d+)";
        html = ''<a href="https://b.tvl.fyi/issues/$1">b/$1</a>'';
      };

      # Auto-link other CLs
      commentlink.gerrit = {
        match = "cl/(\\\\d+)";
        html = ''<a href="https://cl.tvl.fyi/$1">cl/$1</a>'';
      };

      # Configures integration with CAS, which then integrates with a variety
      # of backends.
      auth.type = "OAUTH";
      plugin.gerrit-oauth-provider-cas-oauth = {
        root-url = "https://login.tvl.fyi";
        client-id = "OAUTH-TVL-gerrit-Fv0d8Aizz5";
        # client-secret is set in /var/lib/gerrit/etc/secure.config.
      };

      # Allow users to add additional email addresses to their accounts.
      oauth.allowRegisterNewEmail = true;

      # Use Gerrit's built-in HTTP passwords, rather than trying to use the
      # password against the backing OAuth provider.
      auth.gitBasicAuthPolicy = "HTTP";

      # Email sending (emails are relayed via the tazj.in domain's
      # GSuite currently).
      #
      # Note that sendemail.smtpPass is stored in
      # $site_path/etc/secure.config and is *not* controlled by Nix.
      #
      # Receiving email is not currently supported.
      sendemail = {
        enable = true;
        html = false;
        connectTimeout = "10sec";
        from = "TVL Code Review <tvlbot@tazj.in>";
        includeDiff = true;
        smtpEncryption = "none";
        smtpServer = "localhost";
        smtpServerPort = 2525;
      };
    };
  };

  systemd.services.gerrit = {
    serviceConfig = {
      # There seems to be no easy way to get `DynamicUser` to play
      # well with other services (e.g. by using SupplementaryGroups,
      # which seem to have no effect) so we force the DynamicUser
      # setting for the Gerrit service to be disabled and reuse the
      # existing 'git' user.
      DynamicUser = lib.mkForce false;
      User = "git";
      Group = "git";
    };
  };

  services.depot.restic = {
    paths = [ "/var/lib/gerrit" ];
    exclude = [ "/var/lib/gerrit/tmp" ];
  };
}
