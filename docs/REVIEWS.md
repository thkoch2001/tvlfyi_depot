TVL Code Reviews
================

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [TVL Code Reviews](#tvl-code-reviews)
    - [Gerrit setup](#gerrit-setup)
    - [Gerrit workflows](#gerrit-workflows)
    - [Review process & approvals](#review-process--approvals)
    - [Registration](#registration)
    - [Submitting changes via email](#submitting-changes-via-email)

<!-- markdown-toc end -->


This document describes the TVL code review process & tooling. If you are
looking for general contribution guidelines, please look at the [general
contribution guidelines](./CONTRIBUTING.md).

All changes are tracked at [cl.tvl.fyi](https://cl.tvl.fyi) using Gerrit. See
[Registration](#registration) for information on how to register an account.

## Gerrit setup

Gerrit uses the concept of change IDs to track commits across rebases and other
operations that might change their hashes, and link them to unique changes in
Gerrit.

First, [tell Gerrit][Gerrit SSH] about your SSH keys.

Then, to make using Gerrit smooth for users, the repository should be cloned and
a commit hook should be installed as follows:

```
git clone "ssh://$USER@code.tvl.fyi:29418/depot"
scp -p -P 29418 $USER@code.tvl.fyi:hooks/commit-msg "depot/.git/hooks/"
```

If you have a previous clone of the depot via HTTP you can use `git remote
set-url` to update the origin URL and install the hook in the same way as above.

## Gerrit workflows

The developer workflow on Gerrit is quite different from what GitHub-users are
used to.

The depot does not have branches (other than Gerrit's internal metadata refs)
and all development happens at `HEAD`.

Every time you create a new commit the change hook will insert a unique
`Change-Id` tag into the commit message. Once you are satisfied with the state
of your commit and want to submit it for review, you push it to a git ref called
`refs/for/canon`. This designates the commits as changelists (CLs) targeted for
the `canon` branch.

Sending a change for review is done by pushing to a special target. You can set
this to be the default push target through your git configuration:

```
git config remote.origin.url "ssh://$USER@code.tvl.fyi:29418/depot"
git config remote.origin.push HEAD:refs/for/canon
```

Then, after making your change, push to the default, or to a special target:

```
Example:
git commit -m 'docs(REVIEWS): Fixed all the errors in the reviews docs'
git push origin

# Uploading a work-in-progress CL:
git push origin HEAD:refs/for/canon%wip
```

TIP: Every individual commit will become a separate change. We do not merge
related commits, but instead submit them one by one. Be aware that if you are
expecting a different behaviour and attempt something like an unsquashed subtree
merge, you will produce a *lot* of CLs. This is strongly discouraged.

During your review, the reviewer(s) might ask you to make changes. You can
simply amend your commit(s) and push to the same ref. Gerrit will automatically
update your changes.

Read more about the Gerrit workflow in the [Gerrit walkthrough][].

## Review process & approvals

Each user has the ability to create their own users directory in
`//users/<username>` in which they can submit code without review from other
contributors (they will still need to +2 their own changes, and the initial
check-in of the `OWNERS` file needs to be reviewed).

You can set a directory like this up for yourself by proposing a change similar
to [CL/246](https://cl.tvl.fyi/c/depot/+/246).

For all paths outside of `//users`, code review is required. We have no strict
guidelines about the review process itself, as we're not a megacorp, but we have
formalised checks before submitting:

* At least one person who is [an owner][OWNERS] of the codepath must have given
  a +2 review
* The commit message must conform to our [guidelines][]
* No code review comments must be left unresolved

If all these conditions are fulfilled, the **change author submits their change
themselves**.

## Registration

If you would like to have an account on the Gerrit instance, follow these
instructions:

1. Be a member of `##tvl-dev` or `##tvl`.
2. Clone the depot locally (via `git clone "https://cl.tvl.fyi/depot"`).
3. Create a user entry in our LDAP server in [tvl-slapd/default.nix][tvl-slapd].

   We recommend using ARGON2 password hashes, which can be created
   with the `slappasswd` tool if OpenLDAP was compiled with ARGON2
   support.

   For convenience, we provide a wrapper script for this that you can
   build with `nix-build -A tools.hash-password` in a depot checkout.
   Alternatively, if you have `direnv` installed, you can add the
   depot to your allowlist and just run `hash-password` which should
   be added to your `$PATH` by `direnv`.

   You can probably create ARGON2 hashes with other tools, but that is
   your job to figure out.
4. Create a commit adding yourself (see e.g.
   [CL/223](https://cl.tvl.fyi/c/depot/+/223)).
5. Submit the commit via email (see below).

## Submitting changes via email

You can submit a patch via email to `depot@tazj.in` and it will be added to
Gerrit by a contributor.

Create an appropriate commit locally and send it us using either of these options:

* `git format-patch`: This will create a `.patch` file which you should email to
  us.
* `git send-email`: If configured on your system, this will take care of the
  whole emailing process for you.

The email address is a [public group][].

[Gerrit SSH]: https://cl.tvl.fyi/settings/#SSHKeys
[Gerrit walkthrough]: https://gerrit-review.googlesource.com/Documentation/intro-gerrit-walkthrough.html
[OWNERS]: https://cl.tvl.fyi/plugins/owners/Documentation/config.md
[guidelines]: ./CONTRIBUTING.md#commit-messages
[tvl-slapd]: ../ops/nixos/tvl-slapd/default.nix
[public group]: https://groups.google.com/a/tazj.in/forum/?hl=en#!forum/depot
