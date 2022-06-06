let
  nonremote = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJk+KvgvI2oJTppMASNUfMcMkA2G5ZNt+HnWDzaXKLlo"
  ];

  edwin = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB+OZ8f++cnvd4E2kFyn9jEoVpxi7LfjRvyQwzE8a5Ll"
  ];
in

{
  "warteraum-salt.age".publicKeys = nonremote ++ edwin;
  "warteraum-tokens.age".publicKeys = nonremote ++ edwin;
  "minecraft-rcon.age".publicKeys = nonremote ++ edwin;
}
