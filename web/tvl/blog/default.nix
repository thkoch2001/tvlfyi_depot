{ depot, ... }:

{
  config = {
    name = "TVL's blog";
    footer = depot.web.tvl.footer {};
    baseUrl = "https://tvl.fyi/blog";
  };

  posts = [
    {
      key = "rewriting-nix";
      title = "Tvix: We are rewriting Nix";
      date = 1636038556;
      content = ./rewriting-nix.md;
    }
  ];
}
