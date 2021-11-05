{ depot, ... }:

{
  config = {
    name = "TVL's blog";
    footer = depot.web.tvl.footer {};
    baseUrl = "https://tvl.fyi/blog";
  };

  posts = [
    {
      key = "kicking-off-tvix";
      title = "Kicking off Tvix";
      date = 1636038556;
      content = ./kicking-off-tvix.md;
      draft = true;
    }
  ];
}
