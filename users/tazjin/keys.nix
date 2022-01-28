# My SSH public keys
{ ...
}:
let
  withAll = keys: keys // { all = builtins.attrValues keys; };
in
withAll
  {
    # frog = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKMZzRdcrHTuCPoaFy36MPr5IW/hnImlse/OBOn6udL/ tazjin@frog";
    s10e = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDf7CNlYoauHcSYsMNnCZt5h9QSYH/7keYkg8g3hT32+";
    tverskoy =
      "sk-ecdsa-sha2-nistp256@openssh.com AAAAInNrLWVjZHNhLXNoYTItbmlzdHAyNTZAb3BlbnNzaC5jb20AAAAIbmlzdHAyNTYAAABBBAWvA3RpXpMAqruUbB+eVgvvHCzhs5R9khFRza3YSLeFiIqOxVVgyhzW/BnCSD9t/5JrqRdJIGQLnkQU9m4REhUAAAAEc3NoOg== tazjin@tverskoy";
  }
