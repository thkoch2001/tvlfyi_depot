{ depot, ... }:

depot.web.tvl.template {
  useUrls = true;
  title = "atward";
  content = ''
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

    ### Configuration

    Some behaviour of atward can be configured by adding query
    parameters to the search string:

    * <kbd>cs=true</kbd> - use Sourcegraph instead of cgit to view code


    In some browsers (like Firefox) users can not edit query
    parameters for search engines. As an alternative configuration can
    be supplied via cookies with the same names as the configuration
    parameters.

    The form below can set this configuration:
    <form class="cheddar-callout cheddar-todo">
      <input type="checkbox"
             id="cs-setting"
             name="cs-setting"
             onchange="saveSetting(this, 'cs');">
      <label for="cs-setting">Use Sourcegraph instead of cgit</label>
    </form>

    <noscript>
      <p class="cheddar-callout cheddar-warning">
        The form above only works with Javascript enabled. Only a few
        lines of Javascript are used, and they are licensed under a
        free-software license (MIT).
      </p>
    </noscript>

    ### Source code

    atward's source code lives at
    [//web/atward](https://at.tvl.fyi/?q=%2F%2Fweb%2Fatward).
  '';
  extraHead = ''
    <script>
      /* Initialise the state of all settings. */
      function loadSettings() {
          loadSetting(document.getElementById('cs-setting'), 'cs');
      }

      /* Initialise the state of a setting from a cookie. */
      function loadSetting(checkbox, name) {
          if (document.cookie.split(';').some(function(cookie) {
              return cookie.indexOf(`''${name}=true`) >= 0;
          })) {
              checkbox.checked = true;
          }
      }

      /* Persist the state of a checkbox in a cookie */
      function saveSetting(checkbox, name) {
          console.log(`setting atward parameter '''''${name}' to ''${checkbox.checked.toString()}`);
          document.cookie = `''${name}=''${checkbox.checked.toString()};`;
      }

      document.addEventListener('DOMContentLoaded', loadSettings);
    </script>
    <link rel="search" type="application/opensearchdescription+xml" title="TVL Search" href="https://at.tvl.fyi/opensearch.xml">
  '';
}
