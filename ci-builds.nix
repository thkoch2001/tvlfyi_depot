# This file defines the derivations that should be built by CI.
#
# The "categories" (i.e. attributes) below exist because we run out of
# space on Sourcehut otherwise.
{ depot, lib, ... }:

let
  inherit (builtins) attrNames filter foldl' getAttr substring;

in lib.fix(self: {
  __apprehendEvaluators = throw ''
    Do not evaluate this attribute set directly. It exists only to group builds
    for CI runs of different "project groups".

    To use the depot, always start from the top-level attribute tree instead.
  '';

  # Names of all evaluatable attributes in here. This list will be
  # used to trigger builds for each key.
  __evaluatable = filter (key: (substring 0 2 key) != "__") (attrNames self);

  # List of non-public targets, these are only used in local builds
  # and not in CI.
  __nonpublic = with depot; [
    users.tazjin.nixos.camdenSystem
    users.tazjin.nixos.frogSystem
  ];

  # Combined list of all the targets, used for building everything locally.
  __allTargets = foldl' (x: y: x ++ y) self.__nonpublic
    (map (k: getAttr k self) self.__evaluatable);

  fun = with depot.fun; [
    amsterdump
    gemma
    quinistry
    watchblob
    wcl
  ];

  gitAndFriends = with depot; [
    third_party.cgit
    third_party.git
    web.cgit-taz
  ];

  nix = [ depot.third_party.nix ];

  ops = with depot.ops; [
    depot.ops."posix_mq.rs"
    besadii
    journaldriver
    kms_pass
    kontemplate
    mq_cli
  ];

  various = with depot; [
    depot.web.tvl
    lisp.dns
    nix.buildLisp.example
    nix.yants.tests
    tools.cheddar
    tools.nsfv-setup
    depot.nix.getBins.tests
  ]
  ++ nix.runExecline.tests
  ;

  # User-specific build targets
  tazjin = with depot.users.tazjin; [
    blog
    emacs
    homepage
  ];
})
