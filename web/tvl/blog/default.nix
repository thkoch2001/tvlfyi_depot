{ depot, ... }:

{
  config = {
    name = "TVL's blog";
    footer = depot.web.tvl.footer { };
    baseUrl = "https://tvl.fyi/blog";
  };

  posts = [
    {
      key = "rewriting-nix";
      title = "Tvix: We are rewriting Nix";
      date = 1638381387;
      content = ./rewriting-nix.md;
    }
  ];
}
