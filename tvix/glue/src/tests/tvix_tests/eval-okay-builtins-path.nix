builtins.path {
  name = "little";
  # This needs to be:
  # - a regular file
  # - always present on all systems, and in the Nix sandbox
  # - with predictable contents
  #
  # This is the best I could come up with?
  path = "/sys/kernel/cpu_byteorder";
}
