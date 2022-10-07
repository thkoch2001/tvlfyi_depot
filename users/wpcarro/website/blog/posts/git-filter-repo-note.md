## Background

- I recently used `git-filter-repo` to scrub cleartext secrets from a
  repository.
- We pin some services' deployments to commit SHAs.
- These commit SHAs are no longer reachable from `origin/main`.

## Problem

If the `git` garbage-collects any of the commits to which services are pinned,
and that service attempts to deploy/redeploy, it will fail.

`git for-each-ref --contains $SHA` will report all of the refs that can reach
some commit, `$SHA`. This may be things like:
- `refs/replace`: `git-filter-repo` artifacts
- `refs/stash`
- some local branches
- some remote branches

One solution might involve avoid garbage-collection. But if any of our pinned
commits contained sensitive cleartext we will *want* to ensure that `git` purges
these.

Instead let's find the SHAs of the new, rewritten commits and replace the pinned
versions with those.

## Solution

Essentially we want to find a commit with the same *tree* state as the currently
pinned commit. Here are two ways to get that info...

This way is indirect, but provides more context:

```shell
λ git cat-file -p $SHA
tree d011a1dd4a3c5c4c6455ab3592fa2bf71d551d22 # <-- copy this tree info
parent ba88bbf8de61be932184631244d2ec0ec8205cb8
author William Carroll <wpcarro@gmail.com> 1664993052 -0700
committer William Carroll <wpcarro@gmail.com> 1665116042 -0700

feat(florp): Florp can now flarp

You're welcome :)
```

This way is more direct:

```shell
λ git log -1 --format=%T $SHA
```

Now that we have the SHA of the desired *tree* state, let's query `git` for
commits that share this state.

```shell
λ git log --format='%H %T' | grep $(git log --format=%T -1 $SHA) | awk '{ print $1 }'
```

Hopefully this helps!
