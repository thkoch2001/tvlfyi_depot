---
title: "Let's Learn Nix: Dotfiles"
date: 2020-03-13T22:23:02Z
draft: true
---

## Let's Learn Nix: Dotfiles

### Dependencies

Speaking of dependencies, here's what you should know before reading this tutorial.

- Basic Nix syntax: Nix 1p

What version of Nix are we using? What version of `<nixpkgs>` are we using? What
operating system are we using? So many variables...

Cartesian product of all possibilities...

TODO(wpcarro): Create a graphic of the options.

### The problems of dotfiles

How do you manage your dependencies?

You can use `stow` to install the dotfiles.

### home-manager

What we are going to write is most likely less preferable to the following
alternatives:
- using Nix home-manager
- committing your `.gitconfig` into your

In the next tutorial, we will use [home-manager][wtf-home-mgr] to replace the
functionality that we wrote.

So why bother completing this?

### Let's begin

Welcome to the first tutorial in the [Let's Learn Nix][wtf-lln] series. Today we
are going to create a Nix derivation for one of your dotfiles.

"Dotfiles" refers to a user's collection of configuration files. Typically these
files look like:
- `.vimrc`
- `.xsessionrc`
- `.bashrc`

The leading "dot" at the beginning gives dotfiles their name.

You probably have amassed a collection of dotfiles whether or not you are
aware. For example, if you use [git][wtf-git], the file `~/.gitconfig` should
exist on your machine. You can verify this with:

```shell
$ stat ~/.gitconfig
```

When I was first learning `git`, I learned to configure it using commands I
found in books and tutorials that often looked like:

```shell
$ git config user.email
```

The `~/.gitconfig` file on your machine may look something like this:

```.gitconfig
[user]
	name = John Cleese
	email = john@flying-circus.com
	username = jcleese
[core]
	editor = emacs
[web]
	browser = google-chrome
[rerere]
	enabled = 1
	autoupdate = 1
[push]
	default = matching
[color]
	ui = auto
[alias]
	a = add --all
	ai = add -i
	b = branch
	cl = clone
	cp = cherry-pick
	d = diff
	fo = fetch origin
	lg = log --oneline --graph --decorate
	ps = push
	pb = pull --rebase
	s = status
```

As I ran increasingly more `git config` commands to configure my `git`
preferences, the size of my `.gitconfig` increased, and the less likely I was to
remember which options I set to which values.

Thankfully a coworker at the time, Ryan ([@rschmukler][who-ryan]), told me that
he version-controlled his `.gitconfig` file along with his other configuration
files (e.g. `.vimrc`) in a repository he called "dotfiles".

Version-controlling your dotfiles improves upon a workflow where you have a
variety of configuration files scattered around your machine.

If you look at the above `.gitconfig`, can you spot the dependencies?

We explicitly depend `emacs` and `google-chrome`. We also *implicitly* depend on
`git`: there is not much value of having a `.gitconfig` file if you also do not
have `git` installed on your machine.

Dependencies:
- `emacs`
- `google-chrome`

Let's use Nix to generate this `.gitconfig` file. Here is what I would like our
API to be:

Let's create a file `gitconfig.nix` and build our function section-by-section:

TODO(wpcarro): Link to sections here
- options.user
- options.core
- options.web
- options.rerere
- options.push
- options.color
- options.alias

```shell
$ touch gitconfig.nix
```

### options.user

```haskell
AttrSet -> String
```

```nix
user = {
  name = "John Cleese";
  email = "john@flying-circus.com";
  username = "jcleese";
};
```

```.gitconfig
[user]
	name = John Cleese
	email = john@flying-circus.com
	username = jcleese
```

### options.core

```nix
core = {
  editor = "${pkgs.emacs}/bin/emacs";
};
```

```.gitconfig
[core]
	editor = /nix/store/<hash>-emacs-<version>/bin/emacs
```

### options.web

```nix
web.browser = "${pkgs.google-chrome}/bin/google-chrome";
```

```.gitconfig
[web]
	browser = /nix/store/<hash>-google-chrome-<version>/bin/google-chrome
```

### options.rerere

```nix
rerere = {
  enabled = true;
  autoupdate = true;
};
```

```.gitconfig
[rerere]
	enabled = 1
	autoupdate = 1
```

### options.push

```nix
push.default = "matching";
```

```.gitconfig
[push]
	default = matching
```

### options.color

```nix
color.ui = "auto";
```

```.gitconfig
[color]
	ui = auto
```

We need to define a function named `gitconfig` that creates a Nix [derivation][wtf-derivation]:

```nix
# file: gitconfig.nix
let
  # Import the <nixpkgs> package repository.
  pkgs = import <nixpkgs> {};

  # Stringify the attribute set, `xs`, as a multilined string formatted as "<key> = <value>".
  # See attrsets.nix for more functions that work with attribute sets.
  encodeAttrSet = xs: lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${k} = ${v}") xs);

  # Define out function name `gitconfig` that accepts an `options` argument.
  gitconfig = options: pkgs.stdenv.mkDerivation {
    # The gitconfig file that Nix builds will be located /nix/store/some-hash-gitconfig.
    name = "gitconfig";
    src = pkgs.writeTextFile ".gitconfig" ''
      [user]
          name = ${options.user.name}
          email = ${options.user.email}
          username = ${options.user.username}
      [core]
          editor = ${options.core.editor}
      [web]
          editor = ${options.web.browser}
      [rerere]
          enabled = ${if options.rerere.enabled "1" else "0"}
          autoupdate = ${if options.rerere.autoupdate "1" else "0"}
      [push]
          default = ${options.push.default}
      [color]
          ui = ${options.color.ui}
      [alias]
          ${encodeAttrSet options.aliases}
    '';
    buildPhase = ''
      ${pkgs.coreutils}/bin/cp $src $out
    '';
    installPhase = ''
      ${pkgs.coreutils}/bin/ln -s $out ~/.gitconfig
    '';
  };
} in gitconfig {
  user = {
    name = "John Cleese";
    email = "john@flying-circus.com";
    username = "jcleese";
  };
  core = {
    editor = "${pkgs.emacs}/bin/emacs";
  };
  web.browser = "${pkgs.google-chrome}/bin/google-chrome";
  rerere = {
    enabled = true;
    autoupdate = true;
  };
  push.default = "matching";
  color.ui = "auto";
  aliases = {
	a  = "add --all";
	ai = "add -i";
	b  = "branch";
	cl = "clone";
	cp = "cherry-pick";
	d  = "diff";
	fo = "fetch origin";
	lg = "log --oneline --graph --decorate";
	ps = "push";
	pb = "pull --rebase";
	s  = "status";
  };
}
```

### options.alias

We want to write a function that accepts an attribute set and returns a
string. While Nix is a dynamically typed programming language, thinking in types
helps me clarify what I'm trying to write.

```haskell
encodeAttrSet :: AttrSet -> String
```

I prefer using a Haskell-inspired syntax for describing type signatures. Even if
you haven't written Haskell before, you may find the syntax intuitive.

Here is a non comprehensive, but demonstrative list of example type signatures:
- `[String]`: A list of strings (i.e. `[ "cogito" "ergo" "sum" ]`)
- `AttrSet`: A nix attribute set (i.e. `{ name = "John Cleese"; age = 80; }`).
- `add :: Integer -> Integer -> Integer`: A function named `add` that accepts
  two integers and returns an integer.

Specifically, we want to make sure that when we call:

```nix
encodeAttrSet {
  a = "add --all";
  b = "branch";
}
```

...it returns a string that looks like this:

```.gitconfig
a = "add --all"
b = "branch"
```


TODO(wpcarro): @tazjin's nix-1p mentions this. Link to it.
Nix has useful functions scattered all over the place:
- `lib.nix`
- `list.nix`
- `lib.attrSet`

But I cannot recall exactly which functions we will need to write
`encodeAttrSet`. In these cases, I do the following:
1. Run `nix repl`.
2. Browse the Nix source code.

Google "nix attribute sets" and find the Github link to `attrsets.nix`.

You should consider repeating this search but instead of searching for
"attribute sets" search for "lists" and "strings". That is how I found the
functions needed to write `encodeAttrSet`. Let's return to our `nix repl`.

Load the nixpkgs set:

```nix
nix-repl> :l <nixpkgs>
Added 11484 variables.
```

Define a test input called `attrs`:

```nix
nix-repl> attrs = { fname = "John"; lname = "Cleese"; }
```

Map the attribute set into `[String]` using `lib.mapAttrsToList`:

```nix
nix-repl> lib.mapAttrsToList (k: v: "${k} = ${toString v}") attrs
[ "fname = John" "lname = Cleese" ]
```

Now join the `[String]` together using `lib.concatStringsSep`:

```nix
nix-repl> lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${k} = ${v}") attrs)
"fname = John\nlname = Cleese"
```

Now let's use this to define our function `encodeAttrSet`:

```nix
# file: gitconfig.nix
encodeAttrSet = xs: lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "${k} = ${v}") xs);
```

### Using nixpkgs search

[Nixpkgs search][wtf-nixpkgs-search].

### Conclusion

We learned how to help ourselves.

- Where does `emacs` exist? What about `google-chrome`? [nixpkgs search][wtf-nixpkgs-search]
- Verify that I have it? [nix REPL][using-nix-repl]

We used Nix to create our first derivation.

[wtf-lln]: /lets-learn-nix
[wtf-git]: https://git-scm.com/
[wtf-derivation]: https://nixos.org/nixos/nix-pills/our-first-derivation.html
[wtf-nixpkgs-search]: https://nixos.org/nixos/packages.html?channel=nixos-19.09
[using-nix-repl]: /using-the-nix-repl
[wtf-home-mgr]: https://github.com/rycee/home-manager
[who-ryan]: https://twitter.com/rschmukler
