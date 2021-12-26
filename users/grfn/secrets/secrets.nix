let
  grfn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMcBGBoWd5pPIIQQP52rcFOQN3wAY0J/+K2fuU6SffjA";
  mugwump = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFE2fxPgWO+zeQoLBTgsgxP7Vg7QNHlrQ+Rb3fHFTomB";
in

{
  "bbbg.age".publicKeys = [ grfn mugwump ];
  "cloudflare.age".publicKeys = [ grfn mugwump ];
}
