[
  (builtins.fetchTree {
    type = "git";
    url = "https://github.com/githubtraining/hellogitworld.git";
    narHash = "sha256-DD2NWSjUVyPksTFLd2Z5nSp2QBCU6empZDgLc6tFCfk=";
  }).outPath
]
