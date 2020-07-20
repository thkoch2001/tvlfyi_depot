# monzo_ynab

Exporting Monzo transactions to my YouNeedABudget.com (i.e. YNAB) account. YNAB
unfortunately doesn't currently offer an Monzo integration. As a workaround and
a practical excuse to learn Go, I decided to write one myself.

This job is going to run N times per 24 hours. Monzo offers webhooks for
reacting to certain types of events. I don't expect I'll need realtime data for
my YNAB integration. That may change, however, so it's worth noting.

## Installation

Like many other packages in this repository, `monzo_ynab` is packaged using
Nix. To install and use, you have two options:

You can install using `nix-build` and then run the resulting
`./result/bin/monzo_ynab`.

```shell
> nix-build . && ./result/bin/monzo_ynab
```

Or you can install using `nix-env` if you'd like to create the `monzo_ynab`
symlink.

```shell
> nix-env -f ~/briefcase/monzo_ynab -i
```

## Deployment

While this project is currently not deployed, my plan is to host it on Google
Cloud and run it as a Cloud Run application. What I don't yet know is whether or
not this is feasible or a good idea. One complication that I foresee is that the
OAuth 2.0 login flow requires a web browser until the access token and refresh
tokens are acquired. I'm unsure how to workaround this at the moment.

For more information about the general packaging and deployment strategies I'm
currently using, refer to the [deployments][deploy] writeup.

[deploy]: ../deploy/README.md
