# Common configuration for Nginx.
{ pkgs, ... }:

{
  config = {
    security.acme = {
      acceptTerms = true;
      defaults.email = "wpcarro@gmail.com";
    };

    services.nginx = {
      enable = true;
      enableReload = true;

      recommendedTlsSettings = true;
      recommendedGzipSettings = true;

      # Log errors to journald (i.e. /dev/log) with debug verbosity.
      logError = "syslog:server=unix:/dev/log debug";

      # for journaldriver
      commonHttpConfig = ''
        log_format json_combined escape=json
        '{'
            '"remote_addr":"$remote_addr",'
            '"method":"$request_method",'
            '"host":"$host",'
            '"uri":"$request_uri",'
            '"status":$status,'
            '"request_size":$request_length,'
            '"response_size":$body_bytes_sent,'
            '"response_time":$request_time,'
            '"referrer":"$http_referer",'
            '"user_agent":"$http_user_agent"'
        '}';

        access_log syslog:server=unix:/dev/log,nohostname json_combined;
      '';

      appendHttpConfig = ''
        add_header Permissions-Policy "interest-cohort=()";
      '';
    };
  };
}
