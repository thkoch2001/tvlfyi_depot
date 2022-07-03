let
  tazjin = [
    # tverskoy
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1fGWz/gsq+ZeZXjvUrV+pBlanw1c3zJ9kLTax9FWQy"

    # zamalek
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDBRXeb8EuecLHP0bW4zuebXp4KRnXgJTZfeVWXQ1n1R"
  ];

  grfn = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMcBGBoWd5pPIIQQP52rcFOQN3wAY0J/+K2fuU6SffjA "
  ];

  sterni = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJk+KvgvI2oJTppMASNUfMcMkA2G5ZNt+HnWDzaXKLlo"
  ];

  sanduny = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOag0XhylaTVhmT6HB8EN2Fv5Ymrc4ZfypOXONUkykTX";
  whitby = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILNh/w4BSKov0jdz3gKBc98tpoLta5bb87fQXWBhAl2I";

  whitbyDefault.publicKeys = tazjin ++ grfn ++ sterni ++ [ whitby ];
  allDefault.publicKeys = tazjin ++ grfn ++ sterni ++ [ sanduny whitby ];
in
{
  "besadii.age" = whitbyDefault;
  "buildkite-agent-token.age" = whitbyDefault;
  "buildkite-graphql-token.age" = whitbyDefault;
  "buildkite-ssh-private-key.age" = whitbyDefault;
  "clbot-ssh.age" = whitbyDefault;
  "clbot.age" = whitbyDefault;
  "depot-replica-key.age" = whitbyDefault;
  "gerrit-queue.age" = whitbyDefault;
  "gerrit-secrets.age" = whitbyDefault;
  "grafana.age" = whitbyDefault;
  "irccat.age" = whitbyDefault;
  "journaldriver.age" = allDefault;
  "keycloak-db.age" = whitbyDefault;
  "nix-cache-priv.age" = whitbyDefault;
  "nix-cache-pub.age" = whitbyDefault;
  "oauth2_proxy.age" = whitbyDefault;
  "owothia.age" = whitbyDefault;
  "panettone.age" = whitbyDefault;
  "smtprelay.age" = whitbyDefault;
  "tf-buildkite.age" = whitbyDefault;
  "tf-glesys.age" = whitbyDefault;
  "tf-keycloak.age" = whitbyDefault;
  "tvl-alerts-bot-telegram-token.age" = whitbyDefault;
}
