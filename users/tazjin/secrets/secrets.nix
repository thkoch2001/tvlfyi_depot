let
  myKeys = import ../keys { };
  allKeys = [
    # local keys
    myKeys.tverskoy_ed25519
    myKeys.zamalek_ed25519
    myKeys.khamovnik_agenix
    # koptevo
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMw2ZfdNZCXCOtbQNT6hztXCIkTcO9MBrOuDqMlmGOYK root@koptevo"
  ];
in
{
  "geesefs-tazjins-files.age".publicKeys = allKeys;
  "miniflux.age".publicKeys = allKeys;
  "tgsa-yandex.age".publicKeys = allKeys;
  "lego-yandex.age".publicKeys = allKeys;
}
