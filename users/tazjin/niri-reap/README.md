niri-reap
=========

Tiny, MIT-licensed companion program for [niri](https://github.com/YaLTeR/niri).

I don't use workspaces in my workflow, but when disconnecting an external
screen, the workspaces that it was displaying are moved to the remaining screen.

This program "reaps" all windows on workspaces except the currently active one,
and moves them all to the current workspace.

## Usage

If you have the full TVL monorepo, just `mg run //users/tazjin/niri-reap`. There
is no configuration, and there are no flags.

If you don't have the TVL monorepo and just want `niri-reap`, do this:

1. Get the code: `git clone https://code.tvl.fyi/depot.git:/users/tazjin/niri-reap.git`
2. Run the code: `cargo run`
