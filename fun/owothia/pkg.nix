{ mkDerivation
, base
, bytestring
, chatter
, containers
, envy
, irc-client
, lens
, lib
, random
, relude
, text
}:
mkDerivation {
  pname = "owothia";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    bytestring
    chatter
    containers
    envy
    irc-client
    lens
    random
    relude
    text
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
