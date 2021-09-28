{ depot, pkgs, ... }:


let
  # https://developers.google.com/search/docs/advanced/structured-data/logo
  structuredData = {
    "@context" = "https://schema.org";
    "@type" = "Organisation";
    url = "https://tvl.su";
    logo = "https://tvl.fyi/static/tvl-animated.svg";
  };
in depot.web.tvl.template {
  title = "TVL (The Virus Lounge) - Software consulting";
  content = builtins.readFile ./content.md;
  useUrls = true; # load resources from tvl.fyi
  extraFooter = "\n|\n © ООО ТВЛ";

  # TODO(tazjin): The `.tvl-logo` thing can probably go in the shared CSS.
  extraHead = ''
    <meta name="description" content="TVL provides technology consulting for monorepos, Nix, and other SRE/DevOps/Software Engineering topics.">
    <script type="application/ld+json">
      ${builtins.toJSON structuredData}
    </script>
      <style>
        .tvl-logo {
          width: 60%;
          display: block;
          margin-left: auto;
          margin-right: auto;
        }
      </style>
  '';
}
