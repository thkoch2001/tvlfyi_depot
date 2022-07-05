{ depot, ... }:

{
  usermod = name: depot.path.origSrc + ("/users/wpcarro/nixos/modules/${name}");
}
