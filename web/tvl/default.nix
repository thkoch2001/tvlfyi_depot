{ depot, lib, pkgs, ... }:

with depot.nix.yants;

let
  inherit (pkgs) graphviz runCommandNoCC writeText;
  inherit (depot.web) atom-feed blog tvl;

  listPosts = defun [ (list blog.post) string ] (posts:
    lib.concatStringsSep "\n" (map (p: "* [${p.title}](blog/${p.key})") posts)
  );

  postRenderingCommands = defun [ (list blog.post) string ] (posts:
    lib.concatStringsSep "\n"
      (map (p: "cp ${blog.renderPost tvl.blog.config p} $out/blog/${p.key}.html") posts)
  );

  tvlGraph = runCommandNoCC "tvl.svg" {
    nativeBuildInputs = with pkgs; [ fontconfig freetype cairo jetbrains-mono ];
  } ''
    ${graphviz}/bin/neato -Tsvg ${./tvl.dot} > $out
  '';

  publishedPosts = filter blog.includePost tvl.blog.posts;

  feed = {
    id = "https://tvl.fyi/";
    title = "TVL blog";
    subtitle = "Thoughts and news from The Virus Lounge";
    authors = [ "tazjin" ]; # TODO(tazjin): Extract from post info

    links = lib.singleton {
      rel = "self";
      href = "https://tvl.fyi/feed.atom";
    };

    entries = map (blog.toFeedEntry tvl.blog.config) publishedPosts;
  };

  atomFeed = writeText "feed.atom" (atom-feed.renderFeed feed);

  homepage = tvl.template {
    title = "The Virus Lounge";
    content = ''
      The Virus Lounge
      ================

      ----------------

      <img class="tvl-logo" src="https://static.tvl.fyi/${depot.web.static.drvHash}/logo-animated.svg"
           alt="Virus with lambda-shaped spike proteins sitting on an armchair">

      Welcome to **The Virus Lounge**. We're a group of people who got
      together in 2020, when we felt that there was not enough
      spontaneous socialising on the internet.

      Because of our shared interests in topics like **build systems**
      and **monorepos** we started working on code together, in our
      monorepo called the *depot*.

      Feel free to explore the tech we have built so far, all our
      systems are linked in the footer.

      We mostly hang out on IRC. You can find us in [`#tvl`][tvl-irc]
      on [hackint][], which is also reachable [via XMPP][hackint-xmpp]
      at [`#tvl@irc.hackint.org`][tvl-xmpp] (sic!).

      Hackint also provide a [web chat][tvl-webchat].

      [tvl-irc]: ircs://irc.hackint.org:6697/#tvl
      [hackint]: https://hackint.org/
      [hackint-xmpp]: https://hackint.org/transport/xmpp
      [tvl-xmpp]: xmpp:#tvl@irc.hackint.org?join
      [tvl-webchat]: https://webirc.hackint.org/#ircs://irc.hackint.org/#tvl

      ----------------

      ## Blog

      Here are the most recent TVL blog posts.

      ${listPosts depot.web.tvl.blog.posts}

      You can also follow our [atom feed](feed.atom).

      ----------------

      ## Where did all these people come from?

      It's pretty straightforward. Feel free to click on people, too.

      <div class="tvl-graph-container">
        <!--
          cheddar leaves HTML inside of HTML alone,
          so wrapping the SVG prevents it from messing it up
        -->
        ${builtins.readFile tvlGraph}
      </div>
    '';
    extraHead = ''
      <style>
        .tvl-graph-container {
          max-width: inherit;
        }

        .tvl-graph-container svg {
          max-width: inherit;
          height: auto;
        }

        .tvl-logo {
          width: 60%;
          display: block;
          margin-left: auto;
          margin-right: auto;
        }
      </style>
    '';
  };
in runCommandNoCC "website" {} ''
  mkdir -p $out/blog
  cp ${homepage} $out/index.html
  ${postRenderingCommands tvl.blog.posts}
  cp ${atomFeed} $out/feed.atom
''
