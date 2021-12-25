let
  tazjin = [
    # tverskoy
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1fGWz/gsq+ZeZXjvUrV+pBlanw1c3zJ9kLTax9FWQy"
  ];

  grfn = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMcBGBoWd5pPIIQQP52rcFOQN3wAY0J/+K2fuU6SffjA "
  ];

  sterni = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJk+KvgvI2oJTppMASNUfMcMkA2G5ZNt+HnWDzaXKLlo"
  ];

  whitby = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNh/w4BSKov0jdz3gKBc98tpoLta5bb87fQXWBhAl2I";

  default.publicKeys = tazjin ++ grfn ++ sterni ++ [ whitby ];
in {
  "besadii.age" = default;
  "buildkite-agent-token.age" = default;
  "buildkite-graphql-token.age" = default;
  "clbot-ssh.age" = default;
  "clbot.age" = default;
  "gerrit-queue.age" = default;
  "gerrit-secrets.age" = default;
  "grafana.age" = default;
  "irccat.age" = default;
  "nix-cache-priv.age" = default;
  "nix-cache-pub.age" = default;
  "owothia.age" = default;
  "panettone.age" = default;
}
