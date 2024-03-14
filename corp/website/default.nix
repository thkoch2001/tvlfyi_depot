{ depot, pkgs, ... }:

let
  # https://developers.google.com/search/docs/advanced/structured-data/logo
  structuredData = {
    "@context" = "https://schema.org";
    "@type" = "Organisation";
    url = "https://tvl.su";
    logo = "https://static.tvl.fyi/latest/logo-animated.svg";
  };

  common = description: {
    extraFooter = "\n|\n © ООО ТВЛ";
    staticUrl = "https://static.tvl.su/latest";

    extraHead = ''
      <meta name="description" content="${description}">
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

        .active-lang {
          color: black;
          font-weight: bold;
        }

        .inactive-lang {
          color: inherit;
        }
      </style>
    '';
  };

  descEn = "TVL provides technology consulting for monorepos, Nix, and other SRE/DevOps/Software Engineering topics.";
  indexEn = depot.web.tvl.template (
    {
      title = "TVL (The Virus Lounge) - Software consulting";
      content = builtins.readFile ./content-en.md;
    }
    // common descEn
  );

  descRu = "TVL предоставляет технологическое консультирование по монорепозиториям, Nix и другим темам SRE/DevOps/Software Engineering.";
  indexRu = depot.web.tvl.template (
    {
      title = "ТВЛ - Монорепозитории, SRE, Nix, программное обеспечение";
      content = builtins.readFile ./content-ru.md;
    }
    // common descRu
  );
in
pkgs.runCommand "corp-website" { } ''
  mkdir -p $out/{en,ru}
  cp ${indexEn} $out/index.html
  cp ${indexEn} $out/en/index.html
  cp ${indexRu} $out/ru/index.html
''
