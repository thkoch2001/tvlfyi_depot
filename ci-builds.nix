# This file defines the derivations that should be built by CI.
#
# The "categories" (i.e. attributes) below exist because we run out of
# space on Sourcehut otherwise.
{ depot, lib, ... }:

let
  inherit (builtins) attrNames filter foldl' getAttr substring;

  # attach a nix expression to a drv so we can “build” it
  # TODO(Profpatsch): instead of failing evaluation if a test fails,
  # we can put the expression of the test failure into $out
  # and continue with the other CI derivations.
  drvify = name: exp: depot.nix.emptyDerivation {
    inherit name;
    owo = lib.generators.toPretty {} exp;
  };

  systemFor = configuration: (depot.third_party.nixos {
    inherit configuration;
  }).system;

in lib.fix (self: {
  __apprehendEvaluators = throw ''
    Do not evaluate this attribute set directly. It exists only to group builds
    for CI runs of different "project groups".

    To use the depot, always start from the top-level attribute tree instead.
  '';

  # Names of all evaluatable attributes in here. This list will be
  # used to trigger builds for each key.
  __evaluatable = filter (key: (substring 0 2 key) != "__") (attrNames self);

  # Combined list of all the targets, used for building everything locally.
  __allTargets =
    (with depot.nix.yants; list drv)
    (foldl' (x: y: x ++ y) [] (map (k: getAttr k self) self.__evaluatable));

  fun = with depot.fun; [
    amsterdump
    clbot
    gemma
    paroxysm
    paroxysm.docker
    quinistry
    watchblob
    wcl
  ];

  ops = with depot.ops; [
    depot.ops."posix_mq.rs"
    besadii
    journaldriver
    kontemplate
    mq_cli
    (systemFor nixos.whitby)
  ];

  third_party = with depot.third_party; [
    apereo-cas
    bufbuild
    cgit
    git
    grpc
    loxy
    nix
    nix.test-vm
    openldap
    rapidcheck
  ];

  gerrit = with depot.third_party.gerrit_plugins; [
    depot.third_party.gerrit
    checks
    owners
  ];

  lisp = with depot.lisp; [
    dns
    klatre
  ];

  various = with depot; [
    nix.buildLisp.example
    nix.yants.tests
    tools.cheddar
    tools.nsfv-setup
    web.cgit-taz
    web.todolist
    web.tvl
    web.panettone
    (drvify "getBins-tests" nix.getBins.tests)
  ]
  ++ nix.runExecline.tests
  ;

  # Haskell packages we've patched locally
  haskellPackages = with depot.third_party.haskellPackages; [
    generic-arbitrary
    hgeometry
    hgeometry-combinatorial
    vinyl
    comonad-extras

    # TODO(grfn): Disabled because of build errors with recent nixpkgs
    # depot.third_party.haskell-language-server.ghc883
  ];

  # User-specific build targets
  tazjin = with depot.users.tazjin; [
    blog.rendered
    emacs
    finito
    homepage
    (systemFor nixos.camden)
    (systemFor nixos.frog)
  ];
})
