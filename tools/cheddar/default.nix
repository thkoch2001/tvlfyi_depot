{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;
  doDoc = false;

  override = x: {
    # Use our custom bat syntax set, which is everything from upstream,
    # plus additional languages we care about.
    BAT_SYNTAXES = "${depot.third_party.bat_syntaxes}";
  };
}
