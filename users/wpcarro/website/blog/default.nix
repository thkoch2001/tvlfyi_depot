{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (builtins) hasAttr filter readFile;
  inherit (depot.web.blog) post includePost renderPost;
  inherit (depot.users) wpcarro;
  inherit (pkgs) runCommandNoCC;

  config = {
    name = "wpcarro's blog";
    baseUrl = "https://wpcarro.dev/blog";
  };

  posts = filter includePost (list post (import ./posts.nix));

  rendered = runCommandNoCC "wpcarros-blog" {} ''
    mkdir -p $out

    ${lib.concatStringsSep "\n" (map (post:
      "cp ${renderPost config post} $out/${post.key}.html"
    ) posts)}
  '';

  formatDate = date: readFile (runCommandNoCC "date" {} ''
    date --date='@${toString date}' '+%B %e, %Y' > $out
  '');

  postsList = pkgs.writeText "index.html" ''
    <div class="max-w-sm md:max-w-prose mx-auto">
      <section class="pt-8 pb-14">
        <p class="font-bold pb-4">Personal blog by <a class="font-bold text-blue-600 hover:underline" href="https://wpcarro.dev">wpcarro</a>.</p>
        <p class="text-gray-500">&gt; Half-baked musings lossily encoded.</p>
        <p class="text-gray-500">&gt; - misc reviewer</p>
      </section>
      <ul>
        ${lib.concatStringsSep "\n" (map (post: ''
          <li class="pb-10">
            <h2 class="text-bold font-2xl ">
              <a class="font-bold text-blue-600 hover:underline" href="${config.baseUrl}/${post.key}.html">
                ${post.title}
              </a>
            </h2>
            <p class="text-gray-500">
              ${formatDate post.date}
            </p>
          </li>
        '') posts)}
      </ul>
    </div>
  '';
in {
  inherit posts rendered config;

  root = runCommandNoCC "wpcarros-blog" {} ''
    mkdir -p $out

    cat ${wpcarro.website.header} \
        ${postsList} \
        ${wpcarro.website.addendum} > $out/index.html
  '';
}
