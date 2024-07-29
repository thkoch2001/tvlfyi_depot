builtins.path {
  name = "little";
  # This needs to be:
  # - a regular file
  # - always present on all systems, and in the Nix sandbox
  # - with predictable contents
  #
  # This is the best I could come up with? Maybe there's something better, but
  # this seems to work. I doubt we'll ever be running on IBM System/Z.
  path = "/sys/kernel/cpu_byteorder";
}
