{ pkgs
, depot
, ...
}:
let
  inherit ( builtins ) readFile;
  inherit ( depot.users ) wpcarro;
  domain = "billandhiscomputer.com";
  globalVars = {
    inherit domain;
    homepage = "https://${ domain }/";
    blog = "https://${ domain }/blog";
    habits = "https://${ domain }/habits";
    github = "https://github.com/wpcarro";
    linkedin = "https://linkedin.com/in/williampatrickcarroll";
    depotWork = "https://cs.tvl.fyi/depot/-/blob/users/wpcarro";
  };
  renderTemplate = src: vars: pkgs.substituteAll ( globalVars // vars // { inherit src; } );
  withBrand = contentHtml: renderTemplate ./fragments/template.html { inherit contentHtml; };
in
{
  inherit domain renderTemplate withBrand;
  root =
    pkgs.runCommandNoCC
      "wpcarro.dev"
      { }
      ''
      mkdir -p $out

      # /
      cp ${ withBrand ( readFile ( renderTemplate ./fragments/homepage.html { } ) ) } $out/index.html

      # /habits
      mkdir -p $out/habits
      cp -r ${ wpcarro.website.habit-screens } $out/habits/index.html

      # /blog
      cp -r ${ wpcarro.website.blog } $out/blog
      '';
}
