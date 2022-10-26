## Credit

Credit goes to `tazjin@` for this idea :)

## Background

Using `git` revisions to pin versions is nice, but git SHAs aren't very
human-friendly:

- They're difficult to type.
- They're difficult to say in conversation.
- They're difficult to compare. e.g. Which is newer? `2911fcd` or `db6ac90`?

## Solution

Let's assign monotonically increasing natural numbers to each of
our repo's mainline commits and create `git` refs so we can use references like
`r/123` instead of `2911fcd`.

- They're easy to type: `r/123`
- They're easy to say in conversion: "Check-out rev one-twenty-three."
- They're easy to compare: `r/123` is an earlier version than `r/147`.

## Backfilling

Let's start-off by assigning "revision numbers" as refs for each of the mainline
commits:

```shell
for commit in $(git rev-list --first-parent HEAD); do
  git update-ref "refs/r/$(git rev-list --count --first-parent $commit)" $commit
done
```

We can verify with:

```shell
λ git log --first-parent --oneline
```

If everything looks good, we can publish the refs to the remote:

```shell
λ git push origin 'refs/r/*:refs/r/*'
```

## Staying Current

In order to make sure that any newly merged commits have an associated revision
number as a ref, add something like the following to your CI system to run on
the builds of your repo's mainline branch:

```shell
λ git push origin "HEAD:refs/r/$(git rev-list --count --first-parent HEAD)"
```

## Summary

To verify that the remote now has the expected refs, we can use:

```shell
λ git ls-remote origin | less # grep for refs/r
```

If that looks good, you should now be able to *manually* fetch the refs with:

```shell
λ git fetch origin 'refs/r/*:refs/r/*'
```

Or you can use `git config` to automate this:

```shell
λ git config --add remote.origin.fetch '+refs/r/*:refs/r/*'
λ git fetch origin
```

Now you can run fun commands like:

```shell
λ git show r/1234
λ git diff r/123{4,8} # see changes from 1234 -> 1238
```

Thanks for reading!
