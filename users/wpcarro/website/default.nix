{ pkgs, depot, ... }:

rec {
  inherit (depot.users) wpcarro;

  header = "${./fragments/header.html}";
  footer = "${./fragments/footer.html}";
  addendum = "${./fragments/addendum.html}";

  root = pkgs.stdenv.mkDerivation {
    name = "wpcarro.dev";
    src = builtins.path { path = ./.; name = "website"; };
    installPhase = ''
      mkdir -p $out

      cat ${header} \
          ${./fragments/homepage.html} \
          ${footer} \
          ${addendum} > $out/index.html

      mkdir -p $out/habits
      cp -r ${wpcarro.website.habit-screens} $out/habits/index.html

      cp -r ${wpcarro.website.blog.root} $out/blog
    '';
  };
}
