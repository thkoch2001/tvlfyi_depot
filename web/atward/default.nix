{ depot, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;
  override = x: { ATWARD_INDEX_HTML = depot.web.atward.indexHtml; };
}
