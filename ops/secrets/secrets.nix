let
  tazjin = [
    # tverskoy
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1fGWz/gsq+ZeZXjvUrV+pBlanw1c3zJ9kLTax9FWQy"
  ];

  whitby = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNh/w4BSKov0jdz3gKBc98tpoLta5bb87fQXWBhAl2I";

  default.publicKeys = tazjin ++ [ whitby ];
in {
  "besadii.age" = default;
  "buildkite-agent-token.age" = default;
  "clbot.age" = default;
  "gerrit-queue.age" = default;
  "owothia.age" = default;
}
