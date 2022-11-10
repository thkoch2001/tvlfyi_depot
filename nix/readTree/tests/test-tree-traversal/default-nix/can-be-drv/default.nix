{ ... }:
derivation {
  name = "im-a-drv";
  system = builtns.currentSystem;
  builder = "/bin/sh";
  args = [ "-c" ''echo "" > $out'' ];
}
