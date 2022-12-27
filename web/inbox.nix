# landing page for inbox.tvl.su

{ depot, ... }:

depot.web.tvl.template {
  title = "TVL's public inbox";

  # not hosted on whitby, so we need /latest
  staticUrl = "https://static.tvl.su/latest";

  extraHead = ''
    <link rel="alternate" type="application/atom+xml" href="https://inbox.tvl.su/depot/new.atom" />
  '';

  content = ''
    TVL's public inbox
    ==================

    This is the [public-inbox][] for [The Virus Lounge][TVL]. It is
    essentially like a pull-based mailing list, where we discuss
    anything related to our software or organisation, as well as
    receive patches from external users.

    ## Posting to the inbox

    Anyone can send messages to the inbox by emailing
    **depot@tvl.su**.

    ## Accessing the inbox

    There are several ways to access the inbox, depending on what is
    most convenient for your personal email workflow.

    ### Web browser

    Go to [`/depot/`][inbox-html] to read the inbox in your web
    browser. This is the easiest way to access messages, and with an
    email client supporting `mailto:` links you can respond to
    messages from there, too.

    ### IMAP

    The inbox is available via IMAP:

    **Server:** `inbox.tvl.su`

    **Port:** `993` (TLS enabled)

    **Inbox:** `su.tvl.depot.0` (auto-discoverable)

    You can use *any* credentials to log in, for example the username
    `anonymous` with the password `kittens`. The server will just
    ignore it.

    TIP: There is a wrapper script in `//tools/fetch-depot-inbox` in
    the TVL depot which you can use to synchronise the maildir to your
    computer, which works for email clients like `notmuch`.

    ### Atom feed

    An Atom feed [is available][feed] and should work with your
    favourite feed reader.

    ### NNTP

    News readers can access the inbox via NNTP:

    **Server:** `inbox.tvl.su`

    **Port:** `563` (TLS enabled)

    **Group:** `su.tvl.depot.0` (auto-discoverable)

    No credentials are required to access the server.

    [public-inbox]: https://public-inbox.org/README.html
    [TVL]: https://tvl.fyi
    [inbox-html]: https://inbox.tvl.su/depot/
    [feed]: https://inbox.tvl.su/depot/new.atom
  '';
}
