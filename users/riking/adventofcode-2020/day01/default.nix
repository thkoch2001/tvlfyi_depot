{ depot, ... }:

with depot.third_party;

naersk.buildPackage {
  src = ./.;

  buildInputs = [ ];
  doCheck = true;
}
