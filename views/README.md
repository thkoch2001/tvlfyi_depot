depot views
===========

This folder contains external views of depot content, defined using
josh workspaces. See the individual views for a description of their
individual content and usage information.

Testing changes locally
-----------------------

Generally, when iterating on these files, it's best to locally invoke `josh-
filter` (from `//third_party//josh`) locally to inspect how the workspace would
look like:

  - Commit your changes. This is required, as `josh-filter` operates on your
    `HEAD`, not working directory state.
  - Invoke `josh-filter` with the filter expression,
    for example `josh-filter ':workspace=views/kit'`.
  - Peek at the synthesized git history by looking at `FILTERED_HEAD`.

Testing changes in Gerrit
-------------------------

It's also possible to clone resulting workspaces for CLs that were already
pushed to Gerrit, but didn't land in master yet.

For CL1234 at revision 2, the URL passed to `git clone` would look like this:

```
https://code.tvl.fyi/depot.git@refs/changes/32/1234/2:workspace=views/kit.git
````
