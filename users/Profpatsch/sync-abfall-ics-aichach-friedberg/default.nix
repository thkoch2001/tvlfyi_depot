{depot, pkgs, ...}:

let
  sync-to-dir = depot.users.Profpatsch.writers.python3 {
    name = "sync-ics-to-dir";
    libraries = (py: [
      py.httpx
      py.icalendar
    ]);
  } ./sync-ics-to-dir.py;

in sync-to-dir
