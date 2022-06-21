{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;
  doDoc = false;
  doCheck = true;

  override = x: {
    # Use our custom bat syntax set, which is everything from upstream,
    # plus additional languages we care about.
    BAT_SYNTAXES = "${depot.third_party.bat_syntaxes}";
  };

  passthru = {
    # Wrapper for cgit which can't be told to pass arguments to a filter
    about-filter = pkgs.writeShellScriptBin "cheddar-about" ''
      exec ${depot.tools.cheddar}/bin/cheddar --about-filter $@
    '';
  };

  meta.ci.targets = [
    "about-filter"
  ];
}
