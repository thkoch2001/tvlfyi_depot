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

    {
      key = "tvix-status-202209";
      title = "Tvix Status Update 20220909";
      date = 1662995534;
      content = ./tvix-status-202209.md;
    }
  ];
}
