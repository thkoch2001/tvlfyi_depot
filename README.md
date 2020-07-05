<<<<<<< HEAD   (f54a48 chore(whitby): add lukegb to trusted-users for remote builds)
depot
=====

[![builds.sr.ht status](https://builds.sr.ht/~tazjin/depot/master.svg)](https://builds.sr.ht/~tazjin/depot/master?)

This repository is the [monorepo][] for the community around [tazjin's virus
lounge][tvl], containing our personal tools and infrastructure. Everything in
here is built using [Nix][].

If you've ended up here and have no idea who I am, feel free to follow me [on
Twitter][].

# Highlights

## Tools

* `tools/emacs` contains my personal Emacs configuration (packages & config)
* `fun/aoc2019` contains solutions for a handful of Advent of Code 2019
  challenges, before I ran out of interest
* `tools/blog_cli` contains my tool for writing new blog posts and storing them
  in the DNS zone
* `tools/cheddar` contains a source code and Markdown rendering tool
  that is integrated with my cgit instance to render files in various
  views
* `ops/kontemplate` contains my Kubernetes resource templating tool (with which
  the services in this repository are deployed!)
* `ops/besadii` contains a tool that runs as the git
  `post-receive`-hook on my git server to trigger builds on sourcehut.
* `third_party/nix` contains my fork of the Nix package manager

## Packages / Libraries

* `nix/buildGo` implements a Nix library that can build Go software in the style
  of Bazel's `rules_go`. Go programs in this repository are built using this
  library.
* `nix/buildLisp` implements a Nix library that can build Common Lisp
  software. Currently only SBCL is supported. Lisp programs in this
  repository are built using this library.
* `tools/emacs-pkgs` contains various Emacs libraries that my Emacs setup uses,
  for example:
  * `dottime.el` provides [dottime][] in the Emacs modeline
  * `nix-util.el` provides editing utilities for Nix files
  * `term-switcher.el` is an ivy-function for switching between vterm buffers
* `net/alcoholic_jwt` contains an easy-to-use JWT-validation library for Rust
* `net/crimp` contains a high-level HTTP client using cURL for Rust

## Services

Services in this repository are deployed on a Google Kubernetes Engine cluster
using [Nixery][].

* `web/blog` and `web/homepage` contain my blog and website setup
  (serving at [tazj.in][])
* `web/cgit-taz` contains a slightly patched version of `cgit` that serves my
  git web interface at [git.tazj.in][]
* `ops/journaldriver` contains a small Rust daemon that can forward logs from
  journald to Stackdriver Logging

## Miscellaneous

Presentations I've given in the past are in the `presentations` folder, these
cover a variety of topics and some of them have links to recordings.

There's a few fun things in the `fun/` folder, often with context given in the
README. Check out my [list of the best tools][best-tools] for example.

# Contributing

If you'd like to contribute to any of the tools in here, please check out the
[contribution guidelines](./docs/CONTRIBUTING.md).

[monorepo]: https://en.wikipedia.org/wiki/Monorepo
[tvl]: https://tvl.fyi
[Nix]: https://nixos.org/nix
[on Twitter]: https://twitter.com/tazjin
[Nixery]: https://github.com/google/nixery
[tazj.in]: https://tazj.in
[git.tazj.in]: https://git.tazj.in
[best-tools]: /about/fun/best-tools/README.md
[dottime]: https://dotti.me
=======
CAS Overlay Template [![Build Status](https://travis-ci.org/apereo/cas-overlay-template.svg?branch=master)](https://travis-ci.org/apereo/cas-overlay-template)
=======================

Generic CAS WAR overlay to exercise the latest versions of CAS. This overlay could be freely used as a starting template for local CAS war overlays.

# Versions

- CAS `6.2.x`
- JDK `11`

# Overview

To build the project, use:

```bash
# Use --refresh-dependencies to force-update SNAPSHOT versions
./gradlew[.bat] clean build
```

To see what commands are available to the build script, run:

```bash
./gradlew[.bat] tasks
```

To launch into the CAS command-line shell:

```bash
./gradlew[.bat] downloadShell runShell
```

To fetch and overlay a CAS resource or view, use:

```bash
./gradlew[.bat] getResource -PresourceName=[resource-name]
```

To list all available CAS views and templates:

```bash
./gradlew[.bat] listTemplateViews
```

To unzip and explode the CAS web application file and the internal resources jar:

```bash
./gradlew[.bat] explodeWar
```

# Configuration

- The `etc` directory contains the configuration files and directories that need to be copied to `/etc/cas/config`.

```bash
./gradlew[.bat] copyCasConfiguration
```

- The specifics of the build are controlled using the `gradle.properties` file.

## Adding Modules

CAS modules may be specified under the `dependencies` block of the [Gradle build script](build.gradle):

```gradle
dependencies {
    compile "org.apereo.cas:cas-server-some-module:${project.casVersion}"
    ...
}
```

To collect the list of all project modules and dependencies:

```bash
./gradlew[.bat] allDependencies
```

### Clear Gradle Cache

If you need to, on Linux/Unix systems, you can delete all the existing artifacts (artifacts and metadata) Gradle has downloaded using:

```bash
# Only do this when absolutely necessary
rm -rf $HOME/.gradle/caches/
```

Same strategy applies to Windows too, provided you switch `$HOME` to its equivalent in the above command.

# Deployment

- Create a keystore file `thekeystore` under `/etc/cas`. Use the password `changeit` for both the keystore and the key/certificate entries. This can either be done using the JDK's `keytool` utility or via the following command:

```bash
./gradlew[.bat] createKeystore
```

- Ensure the keystore is loaded up with keys and certificates of the server.

On a successful deployment via the following methods, CAS will be available at:

* `https://cas.server.name:8443/cas`

## Executable WAR

Run the CAS web application as an executable WAR:

```bash
./gradlew[.bat] run
```

Debug the CAS web application as an executable WAR:

```bash
./gradlew[.bat] debug
```

Run the CAS web application as a *standalone* executable WAR:

```bash
./gradlew[.bat] clean executable
```

## External

Deploy the binary web application file `cas.war` after a successful build to a servlet container of choice.

## Docker

The following strategies outline how to build and deploy CAS Docker images.

### Jib

The overlay embraces the [Jib Gradle Plugin](https://github.com/GoogleContainerTools/jib) to provide easy-to-use out-of-the-box tooling for building CAS docker images. Jib is an open-source Java containerizer from Google that lets Java developers build containers using the tools they know. It is a container image builder that handles all the steps of packaging your application into a container image. It does not require you to write a Dockerfile or have Docker installed, and it is directly integrated into the overlay.

```bash
./gradlew build jibDockerBuild
```

### Dockerfile

You can also use the native Docker tooling and the provided `Dockerfile` to build and run CAS.

```bash
chmod +x *.sh
./docker-build.sh
./docker-run.sh
```
>>>>>>> BRANCH (2e185f Squashed 'third_party/apereo-cas/overlay/' content from comm)
