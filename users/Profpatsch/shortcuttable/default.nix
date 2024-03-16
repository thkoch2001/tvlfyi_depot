{ depot, lib, pkgs, ... }:

let
  # run prog... and restart whenever SIGHUP is received
  #
  # this is useful for binding to a shortcut.
  #
  # Unfortunately, this requires a bunch of workarounds around the semantics of `trap`,
  # but the general idea of bundling subprocesses with `setsid` is somewhat sound.
  runShortcuttable =
    depot.nix.writeExecline "run-shortcuttable" { } [
      "importas"
      "-i"
      "run"
      "XDG_RUNTIME_DIR"
      "if"
      [ "mkdir" "-p" "\${run}/shortcuttable/test" ]
      "getpid"
      "-E"
      "controlpid"
      savePid
      "\${run}/shortcuttable/test/control"
      "$controlpid"

      # start the program
      "background"
      [
        startSaveSID
        "\${run}/shortcuttable/test/running-sid"
        "$@"
      ]

      "trap"
      [
        "SIGHUP"
        [
          "if"
          [ "echo" "got hup" ]
          "if"
          [
            "if"
            [ "echo" "killing our child processes" ]
            "envfile"
            "\${run}/shortcuttable/test/running-sid"
            "importas"
            "-ui"
            "child_sid"
            "pid"
            "foreground"
            [ "ps" "-f" "--sid" "$child_sid" ]
            ctrlCCtrlDSid
            "$child_sid"
          ]
          "if"
          [ "echo" "restarting into" "$@" ]
          "background"
          [
            startSaveSID
            "\${run}/shortcuttable/test/running-sid"
            "$@"
          ]
        ]
        "SIGTERM"
        [
          (killShortcuttable { signal = "TERM"; })
          "\${run}/shortcuttable/test/running-sid"
          "\${run}/shortcuttable/test/exit"
        ]
        "SIGINT"
        [
          (killShortcuttable { signal = "INT"; })
          "\${run}/shortcuttable/test/running-sid"
          "\${run}/shortcuttable/test/exit"
        ]
      ]
      depot.users.Profpatsch.execline.setsid
      "child_sid"
      "getpid"
      "-E"
      "exitpid"
      savePid
      "\${run}/shortcuttable/test/exit"
      "$exitpid"
      "sleep"
      "infinity"
    ];

  killShortcuttable = { signal }: depot.nix.writeExecline "kill-shortcuttable" { readNArgs = 2; } [
    "if"
    [ "echo" "got SIG${signal}, quitting" ]
    "if"
    [
      "envfile"
      "$1"
      "importas"
      "-ui"
      "child_sid"
      "pid"
      "foreground"
      [ "ps" "-f" "--sid" "$child_sid" ]
      ctrlCCtrlDSid
      "$child_sid"
    ]
    "if"
    [ "echo" "killing shortcuttable loop" ]
    "envfile"
    "$2"
    "importas"
    "-ui"
    "trap_pid"
    "pid"
    "foreground"
    [ "ps" "-fp" "$trap_pid" ]
    "kill"
    "--signal"
    signal
    "$trap_pid"
  ];

  savePid = depot.nix.writeExecline "save-pid" { readNArgs = 2; } [
    "if"
    [ "echo" "saving process:" ]
    "if"
    [ "ps" "-fp" "$2" ]
    "if"
    [
      "redirfd"
      "-w"
      "1"
      "$1"
      "printf"
      "pid = %s\n"
      "$2"
    ]
    "$@"
  ];

  # try to kill process, first with SIGTERM then SIGQUIT (in case it’s a repl)
  ctrlCCtrlDSid = depot.nix.writeExecline "ctrl-c-ctrl-d" { readNArgs = 1; } [
    "ifelse"
    "-n"
    [ "kill" "--signal" "TERM" "--" "-\${1}" ]
    [
      "if"
      [ "echo" "could not kill via SIGTERM, trying SIGQUIT …" ]
      "ifelse"
      "-n"
      [ "kill" "--signal" "QUIT" "--" "-\${1}" ]
      [ "echo" "SIGQUIT failed as well, keeping it running" ]
      "$@"
    ]
    "$@"
  ];

  startSaveSID = depot.nix.writeExecline "start-save-sid" { readNArgs = 1; } [
    depot.users.Profpatsch.execline.setsid
    "child_sid"
    "importas"
    "-ui"
    "child_sid"
    "child_sid"
    "if"
    [ "echo" "children sid:" "$child_sid" ]
    savePid
    "$1"
    "$child_sid"
    "$@"
  ];


in
runShortcuttable
