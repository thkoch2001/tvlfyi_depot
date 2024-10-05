{ depot, pkgs, ... }:

let
  sync-to-dir = depot.users.Profpatsch.writers.python3
    {
      name = "sync-ics-to-dir";
      libraries = (py: [
        py.httpx
        py.icalendar
      ]);
    } ./sync-ics-to-dir.py;

  config =
    depot.users.Profpatsch.importDhall.importDhall
      {
        root = ./..;
        files = [
          "sync-abfall-ics-aichach-friedberg/ics-to-caldav.dhall"
          "dhall/lib.dhall"
          "ini/ini.dhall"
        ];
        main = "sync-abfall-ics-aichach-friedberg/ics-to-caldav.dhall";
        deps = [
        ];
      }
      depot.users.Profpatsch.ini.externs;



in
{ inherit config; }
