let
  grfn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMcBGBoWd5pPIIQQP52rcFOQN3wAY0J/+K2fuU6SffjA";
  mugwump = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFE2fxPgWO+zeQoLBTgsgxP7Vg7QNHlrQ+Rb3fHFTomB";
  bbbg = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL/VzrNEY47KPTce3dgfORkAbweWkr4BI8j54BAIs7bG";
in

{
  "bbbg.age".publicKeys = [ grfn mugwump bbbg ];
  "cloudflare.age".publicKeys = [ grfn mugwump ];
}
