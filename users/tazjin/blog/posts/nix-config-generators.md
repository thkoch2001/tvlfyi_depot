One of the hallowed traditions of writing a new NixOS module that adds support
for running some software on NixOS is inventing a DSL for generating that
program's config files.

Lets look at two quick examples:

## OpenLDAP

Modern versions of OpenLDAP are configured using [an entry in the LDAP
server][slapd-config] itself. This is commonly done by writing LDIF, a
plain-text format for serialising LDAP data. Here's an example from the official
OpenLDAP documentation:

```
# set a rootpw for the config database so we can bind.
# deny access to everyone else.
dn: olcDatabase=config,cn=config
objectClass: olcDatabaseConfig
olcDatabase: config
olcRootPW: {SSHA}XKYnrjvGT3wZFQrDD5040US592LxsdLy
olcAccess: to * by * none
```

Anyone who has experience using (Open)LDAP on any system can figure out what
this means, and reference freely available config documentation and examples to
expand on it.

NixOS ships with a handy module for OpenLDAP which lets us configure an LDAP
server using configuration as simple as `services.openldap.enable = true;`.

What does it look like however if we want to use the above configuration?
Approximately like this:

```nix
{
  services.openldap = {
    enable = true;

    settings.children = {
      "olcDatabase=config".attrs = {
        olcDatabase = "config";
        olcRootPW = "{SSHA}XKYnrjvGT3wZFQrDD5040US592LxsdLy";
        olcAccess = "to * by * none";
      };
    };
  };
}
```

I say "approximately" because it's difficult to understand from the
documentation how exactly the translation from Nix data structures to LDIF
works.

We now have a format that mixes syntaxes (config values are still what you would
have inside a literal LDIF file), but with an intermediate layer on top that
which the administrator needs to learn. This new format has basically no
available examples online and doesn't match what users would find in the
OpenLDAP documentation, necessitating an expensive mental translation process.

## public-inbox

Okay, one more for good measure. There is a NixOS service for [public-inbox][],
a suite of programs that can be used to make a single email inbox publicly
available through various different archive formats. We use this for [TVL's
mailing list][tvl-inbox].

public-inbox is configured using an INI file, an incomplete example might look
like this:

```ini
[publicinbox]
	wwwlisting = all
	imapserver = inbox.tvl.su

[publicinbox "depot"]
	address = depot@tvl.su
	url = https://inbox.tvl.su/depot
	inboxdir = /var/lib/public-inbox/depot
```

How do you represent this in the NixOS configuration using the public-inbox
module of NixOS?

Well, it turns out that if you are reading this before the merge of
[nixpkgs#207693][] ... you can't! Lets look at how it *should* work:

```nix
{
  services.public-inbox = {
    enable = true;

    inboxes.depot = {
      address = [ "depot@tvl.su" ];
      url = "https://inbox.tvl.su/depot";
      inboxdir = "/var/lib/public-inbox/depot";
    };

    settings.publicinbox = {
      wwwlisting = "all";
      imapserver = "inbox.tvl.su";
    };
  };
}
```

Building this configuration without the above pull request results in:

```
error: A definition for option `services.public-inbox.settings.publicinbox.imapserver' is not of type `attribute set of (INI atom (null, bool, int, float or string) or a list of them for duplicate keys)'. Definition values:
- In `/depot/ops/modules/depot-inbox.nix': "inbox.tvl.su"
```

Removing the `imapserver` option generates the config file as expected, just
with that option missing.

What's going on here is that the public-inbox module has a *partial*
representation of the available options of `public-inbox` in its NixOS module,
and a so called "freeform type" as the escape hatch for settings that it doesn't
know about. This freeform type has the following definition at nixpkgs commit
`4ec86b13c9b`:

```nix
let
  gitIni = pkgs.formats.gitIni { listsAsDuplicateKeys = true; };
  iniAtom = elemAt gitIni.type/*attrsOf*/.functor.wrapped/*attrsOf*/.functor.wrapped/*either*/.functor.wrapped 0;
  freeformType = with types; /*inbox name*/attrsOf (/*inbox option name*/attrsOf /*inbox option value*/iniAtom);
in
 # ...
```

If you don't know what that means - I don't blame you. I've written [an
evaluator][] for the Nix language and I still had to put in some mental work to
make sense of this. The core problem is that this escape hatch only allows
*nested* extra configurations, for example this config would be valid:

```
settings.publicinbox.depot.hello = "world";

=> generating this config:

[publicinbox "depot"]
hello = world
```

This is because the type of the freeform escape hatch does not actually match
the type of the data structure that is being serialised. Ironically there is no
escape hatch from the escape hatch, so short of resorting to injecting shell
scripts that modify the generated config files there is nothing users can do to
work around this.

## Okay, so what's the problem?

The problem, as I see it, is this: The practice of writing opaque generators
that transform Nix data structures into arbitrary configuration formats is
error-prone and unnecessary.

The motivation behind these config generators is usually one of:

1. Providing users with a single familiar language (Nix) to write all
   configuration.

2. Adding "type checks" using the module system for configuration options set by
   users.

I believe that both of these are misguided. Nix is not actually a language that
anyone except the most experienced users really knows, and for most users
(especially when configuring familiar software) writing the same configuration
formats they are used to from other systems will feel *more* familiar.

Nix is also inadequate for expressing useful type checks over most program
configuration. We are able to enforce adherence to primitive types, and people
might even go so far as to write regular expressions matching specific expected
values, but as a general rule it can be said that **fully validating an
arbitrary program's configuration requires reimplementing that program**.

These generators also have a variety of other drawbacks, in no particular order:

* They are often insufficiently documented and unnecessarily complex.
* In many cases, their implementation makes it difficult to preview the
  generated config without building the entire NixOS system using it.
* It is no longer possible for users to rely on external documentation for the
  configuration of the software they are trying to use.
* They often lack critical functionality that their author simply has not run
  into, but that might show up quickly in other people's use-cases.

Unfortunately, [RFC 42][rfc42] recommends that all configuration should be moved
in this direction and that escape hatches allowing users to write familiar
configuration should be removed.

I can't express enough how much I consider this to be an **enormous mistake**.
Even for experienced Nix users, using these DSLs is *hard* and it makes adoption
of NixOS significantly more difficult for new users.

## What can be done?

For this particular topic, the ship has probably sailed for now and we have to
be ready for a large amount of [toil][] (and bugs) as more existing modules move
in this direction.

I ask module authors to consider the problems listed above, and make sure that
they provide a real escape hatch that lets users skip the DSLs and just
configure their programs without overhead.

The only way that this should ever be done is by providing users with
*functions* that can generate the appropriate configuration, which they can
*call themselves* to pass the results to the appropriate NixOS options.

[slapd-config]: https://www.openldap.org/doc/admin24/slapdconf2.html
[public-inbox]: https://public-inbox.org/README.html
[tvl-inbox]: https://inbox.tvl.su/depot
[nixpkgs#207693]: https://github.com/NixOS/nixpkgs/pull/207693
[an evaluator]: https://tvixbolt.tvl.su
[rfc42]: https://github.com/NixOS/rfcs/blob/master/rfcs/0042-config-option.md
[toil]: https://sre.google/sre-book/eliminating-toil/
