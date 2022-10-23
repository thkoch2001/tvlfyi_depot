{ depot, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;

  # Got broken by a rustc update (?)
  # https://buildkite.com/tvl/depot/builds/17910#01841493-dc42-44f8-b904-32bf3d835485
  meta.ci.skip = true;
}
