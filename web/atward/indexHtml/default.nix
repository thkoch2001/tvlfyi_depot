{ depot, ... }:

depot.web.tvl.template {
  title = "atward";
  content = ''
    atward
    ======

    ----------

    **atward** is [TVL's](https://tvl.fyi/) search
    service. It can be configured as a browser search engine for easy
    access to TVL bugs, code reviews, code paths and more.

    ### Setting up atward

    To configure atward, add a search engine to your browser with the
    following search string: `https://at.tvl.fyi/?q=%s`
    Consider setting a shortcut, for example **t** or **tvl**.
    You can now quickly access TVL resources by typing something
    like <kbd>t b/42</kbd> in your URL bar to get to the bug with ID
    42.


    ### Supported queries

    The following query types are supported in atward:

    * <kbd>b/42</kbd> - access bugs with ID 42
    * <kbd>cl/3087</kbd> - access changelist with ID 3087
    * <kbd>//web/atward</kbd> - open the **//web/atward** path in TVLs monorepo
    * <kbd>r/3002</kbd> - access revision 3002 in cgit

    When given a short host name (e.g. <kbd>todo</kbd> or
    <kbd>cl</kbd>), atward will redirect to the appropriate `tvl.fyi`
    domain.

    ### Source code

    atward's source code lives at
    [//web/atward](https://at.tvl.fyi/?q=%2F%2Fweb%2Fatward).
  '';

  extraHead = ''
    <link rel="search" type="application/opensearchdescription+xml" title="TVL Search" href="https://at.tvl.fyi/opensearch.xml">
  '';
}
