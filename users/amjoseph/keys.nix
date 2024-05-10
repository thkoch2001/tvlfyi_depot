{ ... }:

let
  # Long-term, air-gapped PGP key.  This key is used only for signing other
  # keys.  It is a minor hassle for me to access this key.
  airgap = "F0B74D717CDE8412A3E0D4D5F29AC8080DA8E1E0";

  # Stored in an HSM.  Signed by the above key.
  current = "D930411B675A011EB9590713DC4AB809B13BE76D";

  # Chat protocols that depend on DNS, WebPKI, or E.164 are lame.  This is not.
  ricochet = "emhxygy5mezcovm5a6q5hze5eqfqgieww56eh4ttwmrolwqmzgb6qiyd";

  # This ssh key is for depot.  Please don't use it elsewhere, except to give
  # me the ability to set a system-specific key elsewhere.  Not currently
  # stored in an HSM, but I'm working on that.
  ssh-for-depot = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOE5e0HrwQTI5KOaU12J0AJG5zDpWn4g/U+oFXz7SkbD";

in {
  all = [ ssh-for-depot ];
}
