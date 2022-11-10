{ localSystem, ... }:
derivation {
  name = "im-a-drv";
  system = localSystem;
  builder = "/bin/sh";
  args = [ "-c" ''echo "" > $out'' ];
}
