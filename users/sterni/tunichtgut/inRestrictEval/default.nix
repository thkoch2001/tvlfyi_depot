# Returns true if it is being evaluated using restrict-eval, false if not; It's
# more robust than using `builtins.getEnv` since it isn't fooled by `env -i`.
#
# See https://github.com/NixOS/nix/issues/6579, haven't checked yet, what causes
# this behavior precisely.
{ ... }:

builtins.pathExists (toString ./guinea-pig + "/.")
