---
title: "Lets Learn Nix"
date: 2020-03-13T21:50:47Z
draft: false
---

## Background

[Nix][wtf-nix] may be the most useful tool that I use. I consider it as valuable
as [Git][wtf-git] or [Emacs][wtf-emacs]. My friend, David ([@dmjio][who-dmjio]),
first introduced me to Nix when we worked together at a Haskell startup in
NYC. Before this, I had been managing my system configuration using software
that I wrote -- first in Bash, then in Python, then in Golang.

It took me awhile to understand Nix. I left the NYC startup, joined Google, and
relocated to London. Here I met another Nix-enlightened monk, Vincent
([@tazjin][who-tazjin]), who patiently taught me enough Nix to become
self-reliant and productive.

Many resources exist to learn Nix; the Nix community on IRC continues to help me
and others effectively use Nix. I'm creating this series to write the tutorials
that I would have found useful when I started learning Nix. If you are just
beginning your Nix journey, I hope these tutorials help you.

## Goals

I aim to make each tutorial in the "Let's Learn Nix" series:
- Actionable: Readers will be writing code.
- Digestible: Readers should be able to finish each tutorial in fifteen minutes.
- Reproducible: Readers should expect the output of their code to match what
  these tutorials claim they should see.

## About the author

My name is William ([@wpcarro][who-wpcarro]). My three favorite tools are Git,
Emacs, and Nix. I am an American expat currently working at Google in London,
UK. While during the day I primarily write Java, Python, and TypeScript, I
prefer functional programming. I use Nix to deploy software and manage the
multiple machines across which I work.

## Let's Begin

Before we get started, Nix is a programming language. To familiarize yourself
with the syntax, semantics, and idioms, consider reading this brief [Nix One
Pager][nix-1p]. I recommend keeping it around as a reference.

When I was first learning Nix, I wanted to use it to manage my dotfiles. Our
first tutorial will help you get started: [Let's Learn Nix:
Dotfiles][lln-dotfiles]

[wtf-nix]: https://nixos.org
[wtf-git]: https://git-scm.com
[wtf-emacs]: https://www.gnu.org/software/emacs
[who-dmjio]: https://twitter.com/dmjio
[who-tazjin]: https://twitter.com/tazjin
[who-wpcarro]: https://twitter.com/wpcarro
[lln-dotfiles]: /lets-learn-nix-dotfiles
[nix-1p]: https://github.com/tazjin/nix-1p
