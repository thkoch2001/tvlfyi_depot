## Background

Sometimes you need to merge one Git repo into another. This is a common task
when administrating a monorepo.

Here's a checklist that I follow:

1. Detect leaked secrets.
1. Rotate leaked secrets.
1. Purge leaked secrets from repo history.
1. Create mainline references to branches (for deployments).
1. Subtree-merge into the target repo.
1. Format the code.
1. Celebrate!

## Secrets

**Note:** If you notice any leaked secrets, first and foremost rotate them
before moving on...

`gitleaks` supports `gitleaks protect`, but that doesn't seem to work for `WRN`
level leaks, which in my experience often contain sensitive cleartext. We can
use `git-filter-repo` to purge the cleartext from our repo history.

Let's make a `secrets.txt` file that we can feed `git-filter-repo`:

```shell
λ gitleaks detect -r /tmp/secrets.json
λ jq -r 'map_values(.Secret) | .[]' /tmp/secrets.txt
```

Now for the redacting...

```shell
λ git-filter-repo --force --replace-text /tmp/secrets.txt
```

Verify that the secrets were removed.

```shell
λ rg --hidden '\*\*\*REMOVED\*\*\*'
λ gitleaks detect -v
```

Looks good! Let's move on to support the adopted repo's deploy strategy.

## Supporting Deploys

While deploying services when someone pushes to a given branch is a common
deployment strategy, branch-based deployment don't make a whole lot of sense in
a monorepo.

When adopting another repo, you'll typically encounter a Github Action
configuration that contains a section like this:

```yaml
on:
  push:
    - staging
    - production
```

In our monorepo, `staging` and `production` don't exist. And I don't think we
want to support them either. `staging` and `production` are ambiguous in a
monorepo that hosts multiple services each of which likely having its own notion
of `staging` and `production`.

Doing "pinned releases" where a service is deployed from a `git` revision from
the mainline branch works well in these scenarios. In order to support this we
need to make sure the adopted repo has references to

`git subtree add` asks us to define which branch it should use when grafting the
repository onto our monorepo. We'll use `main` (or whatever the mainline branch
is).

In order to support the *current* deployments while migrating to a pinned
release strategy, we have to ensure that `main` has a commit containing the same
tree state as `staging` *and* another commit containing the same tree state as
`production`. Let's do that!

```shell
λ git checkout main # ensure you're on the main branch
λ git diff main staging >/tmp/main-to-staging.patch
λ git diff main production >/tmp/main-to-production.patch
```

### staging

```shell
λ git apply /tmp/main-to-staging.patch
λ git add . && git commit # chore: main -> staging
λ git revert HEAD
λ git commit --amend # revert: staging -> main
```

### production

```shell
λ git apply /tmp/main-to-production.patch
λ git add . && git commit # chore: main -> production
λ git revert HEAD
λ git commit --amend # revert: production -> main
```

Now let's check our work:

```shell
λ git log --oneline
38f4422 revert: production -> main
f071a9f chore: main -> production
02ea731 revert: staging -> main
308ed90 chore: main -> staging
```

When we go to support pinned releases we can do something like so:

```json
{
  "staging": "308ed90",
  "production": "f071a9f"
}
```

## Subtree Merge

Now the repo is ready to be merged.

```shell
λ git subtree add --prefix=foo/bar/baz path/to/baz main
λ git commit --amend # subtree: Dock baz into monorepo!
```

## Formatting

Some CI enforces code formatting standards, so you may need to run that:

```shell
λ repofmt
λ git add . && git commit # chore(fmt): Format the codes
```

Lastly, if you need the latest monorepo code from `origin/main` before opening a
pull request, the following should work:

```shell
λ git fetch origin main && git rebase origin/main --rebase-merges --strategy=subtree
```
