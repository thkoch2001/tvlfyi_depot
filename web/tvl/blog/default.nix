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
      author = "tazjin";
    }

    {
      key = "tvix-status-september-22";
      title = "Tvix Status - September '22";
      date = 1662995534;
      content = ./tvix-status-202209.md;
      author = "tazjin";
    }
    {
      key = "tvix-architecture-design";
      title = "Tvix Architecture design";
      date = 1663246783;
      content = ./tvix-architecture-design.md;
      draft = true;
      author = "flokli";
    }
  ];
}
