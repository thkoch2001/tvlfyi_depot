<!-- SPDX-License-Identifier: MIT -->
passively
=========

Passively is an Emacs Lisp library for passively learning new
information in an Emacs instance.

Passively works by displaying a random piece of information to be
learned in the Emacs echoline whenever Emacs is idle for a set amount
of time.

It was designed to aid in language acquisition by passively displaying
new vocabulary to learn.

Passively is configured with a corpus of information (a hash table
mapping string keys to string values) and maintains a set of terms
that the user already learned in a file on disk.

## Configuration & usage

Configure passively like this:

```lisp
;; Configure the terms to learn. Each term should have a key and a
;; string value which is displayed.
(setq passively-learn-terms
      (ht ("забыть" "забыть - to forget")
          ("действительно" "действительно - indeed, really")))

;; Configure a file in which passively should store its state
;; (defaults to $user-emacs-directory/passively.el)
(setq passively-store-state "/persist/tazjin/passively.el")

;; Configure after how many seconds of idle time passively should
;; display a new piece of information.
;; (defaults to 4 seconds)
(setq passively-show-after-idle-for 5)

;; Once this configuration has been set up, start passively:
(passively-enable)

;; Or, if it annoys you, disable it again:
(passively-disable)
```

These variables are registered with `customize` and may be customised
through its interface.

### Known terms

Passively exposes the interactive function
`passively-mark-last-as-known` which marks the previously displayed
term as known. This means that it will not be included in the random
selection anymore.

### Last term

Passively stores the key of the last known term in
`passively-last-displayed`.

## Installation

Inside of the TVL depot, you can install passively from
`pkgs.emacsPackages.tvlPackages.passively`. Outside of the depot, you
can clone passively like this:

    git clone https://code.tvl.fyi/depot.git:/tools/emacs-pkgs/passively.git

Passively depends on `ht.el`.

Feel free to contribute patches by emailing them to `depot@tvl.su`.

## Use-cases

I'm using passively to learn Russian vocabulary. Once I've cleaned up
my configuration for that, my Russian term list will be linked here.
