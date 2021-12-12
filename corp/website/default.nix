{ depot, pkgs, ... }:

let
  # https://developers.google.com/search/docs/advanced/structured-data/logo
  structuredData = {
    "@context" = "https://schema.org";
    "@type" = "Organisation";
    url = "https://tvl.su";
    logo =
      "https://static.tvl.fyi/${depot.web.static.drvHash}/logo-animated.svg";
  };
  index = depot.web.tvl.template {
    title = "TVL (The Virus Lounge) - Software consulting";
    content = builtins.readFile ./content.md;
    extraFooter = ''

      |
       © ООО ТВЛ'';

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
  };
in pkgs.runCommandNoCC "corp-website" { } ''
  mkdir $out
  cp ${index} $out/index.html
''
