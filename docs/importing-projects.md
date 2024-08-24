Importing projects into depot
=============================

Before importing an existing `git`-based project into depot, a few questions
need to be answered:


* Is the project licensed under a free software license, or public domain?
* Do you need to import existing history?
* Do you need to export new history with hashes that continue on from the old
  history? (e.g. importing an existing repository, and exporting from depot to
  the old upstream)

Think about this and then pick an approach below:

## Import with no history (just commit)

Simply copy the files to where you want them to be in depot, and commit. Nothing
else to do!

## Import without continuous history (subtree merge)

This import approach lets you drop an existing project into depot, keep its
existing history, but not retain the ability to continue external history.

This means that if you, for example, import a project from a different git host
using this method, and then continue to commit to it inside of depot, you will
not be able to export a history consistent with your previous hashes using
`josh`.

Commit hashes before the import will exist in depot and be valid.

Still, this approach might be viable if a project "moves into" depot, or has
nothing depending on it externally.

1. Pick a location in depot where you want your project to be (`$loc` from now on).
2. Fetch your project into the same git store as your depot clone (e.g. by
   adding it as an upstream and fetching it).
3. Pick the commit you want to merge (`$commit` from now on).
4. Run `git subtree add --prefix=$loc $commit`, which will create the correct
   merge commit.
5. Ensure Gerrit [knows about your commit](#preparing-merges-in-gerrit) for the
   parent that is being merged.
6. Modify the merge commit's message to start with `subtree($project_name):`.
   Gerrit **will not** allow merge commits that do not follow this format.
7. Push your subtree commit for review as normal.

## Import with continuous history

This approach imports the history using `josh`, which means that external
history before/after the import is consistent (you can continue committing in
`depot`, export the history back out, and from an external perspective nothing
changes).

This is what we did with repositories like `nix-1p` and `nixery`.

Note: Inside of depot, the pre-import commit hashes will **not make sense**.
`josh` will rewrite them in such a way that exporting the project will yield the
same hashes, but this rewriting changes the hashes of your commits inside of
depot.

1. Pick a location in depot where you want your project to be (`$loc`).
2. Fetch your project into the same git store as your depot clone (e.g. by
   adding it as an upstream and fetching it).
3. Check out the commit you want to merge into depot.
4. Run `josh-filter ":prefix=$loc"`, and take note of the `FILTERED_HEAD` ref
   that it produces (`$filtered` from now on).
5. Ensure Gerrit [knows about the filtered commit](#preparing-merges-in-gerrit).
6. Merge the filtered commit into depot using a standard merge, but make sure to
   add the `--allow-unrelated-histories` flag. Your commit message **must**
   start with `subtree($project_name):`, otherwise Gerrit will not let you push
   a merge.
7. Push the merge commit for review as usual.

------------------------------------------------------

## Preparing merges in Gerrit

When pushing a merge to Gerrit, it needs to know about all ancestors of the
merge, otherwise it will try to interpret commits as new CLs and reject them for
not having a change ID (or create a huge number of CLs, if they do have one).

To prevent this, we have a special git ref called `subtree-staging` which you
can push external trees to.

Access to `subtree-staging` has to be granted by a TVL admin, so ping tazjin,
lukegb, flokli, sterni and so on before proceeding.

1. Determine the commit you want to merge (`$commit`).
2. Run `git push -f $commit origin/subtree-staging` (or replace `origin` with
   whatever the TVL Gerrit remote is called in your clone).
