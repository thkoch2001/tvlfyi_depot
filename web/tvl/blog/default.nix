{ depot, ... }:

{
  config = {
    name = "TVL's blog";
    footer = depot.web.tvl.footer { };
    baseUrl = "https://tvl.fyi/blog";
  };

  posts = builtins.sort (a: b: a.date > b.date) [
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
      key = "tvix-update-february-24";
      title = "Tvix Status - February '24";
      date = 1707472132;
      content = ./2024-02-tvix-update.md;
      author = "flokli";
    }
    {
      key = "tvix-update-august-24";
      title = "Tvix Status - August '24";
      date = 1723219370;
      content = ./2024-08-tvix-update.md;
      author = "flokli";
      draft = true;
    }
  ];
}
