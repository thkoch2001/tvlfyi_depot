let
  myKeys = import ../keys { };
  allKeys = [
    # local keys
    myKeys.tverskoy_ed25519
    myKeys.zamalek_ed25519
    # koptevo
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMw2ZfdNZCXCOtbQNT6hztXCIkTcO9MBrOuDqMlmGOYK root@koptevo"
  ];
in
{
  "tgsa-yandex.age".publicKeys = allKeys;
  "monica-appkey.age".publicKeys = allKeys;
}
