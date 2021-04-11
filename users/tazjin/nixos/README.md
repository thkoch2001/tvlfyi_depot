NixOS configuration
===================

My NixOS configurations! It configures most of the packages I require
on my systems, sets up Emacs the way I need and does a bunch of other
interesting things.

System configuration lives in folders, and some of the modules stem
from `//ops/modules`.

Machines are deployed with the script at `ops.nixos.rebuild-system`.

## Configured hosts:

* `tverskoy` - X13 AMD that's travelling around with me
* `frog` - weapon of mass computation (in storage in London)
* `camden` - NUC formerly serving tazj.in (in storage in London)
