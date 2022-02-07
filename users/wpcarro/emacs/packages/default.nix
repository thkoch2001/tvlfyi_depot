{ depot, ... }:

let
  inherit (depot.tools.emacs-pkgs) buildEmacsPackage;
in
rec {
  gt = buildEmacsPackage {
    pname = ">";
    version = "1.0";
    src = ./gt.el;
    externaRequires = epkgs: [ ];
  };

  al = buildEmacsPackage {
    pname = "al";
    version = "1.0";
    src = ./al.el;
    externalRequires = epkgs: [
      epkgs.dash
      macros
      tuple
      maybe
    ];
  };

  bag = buildEmacsPackage {
    pname = "bag";
    version = "1.0";
    src = ./bag.el;
    externaRequires = epkgs: [
      al
      number
    ];
  };

  bookmark = buildEmacsPackage {
    pname = "bookmark";
    version = "1.0";
    src = ./bookmark.el;
    externaRequires = epkgs: [
      epkgs.dash
      epkgs.f
      epkgs.general
      buffer
      set
      string
    ];
  };

  buffer = buildEmacsPackage {
    pname = "buffer";
    version = "1.0";
    src = ./buffer.el;
    externaRequires = epkgs: [
      epkgs.ts
      struct
      cycle
      prelude
      maybe
      set
    ];
  };

  bytes = buildEmacsPackage {
    pname = "bytes";
    version = "1.0";
    src = ./bytes.el;
    externaRequires = epkgs: [
      prelude
      math
      number
    ];
  };

  cache = buildEmacsPackage {
    pname = "cycle";
    version = "1.0";
    src = ./cycle.el;
    externaRequires = epkgs: [
      prelude
      struct
      gt
    ];
  };

  clipboard = buildEmacsPackage {
    pname = "clipboard";
    version = "1.0";
    src = ./clipboard.el;
    externaRequires = epkgs: [ ];
  };

  colorscheme = buildEmacsPackage {
    pname = "colorscheme";
    version = "1.0";
    src = ./colorscheme.el;
    externaRequires = epkgs: [
      cycle
      gt
    ];
  };

  cycle = buildEmacsPackage {
    pname = "cycle";
    version = "1.0";
    src = ./cycle.el;
    externaRequires = epkgs: [
      prelude
      math
      maybe
      struct
    ];
  };

  display = buildEmacsPackage {
    pname = "display";
    version = "1.0";
    src = ./display.el;
    externaRequires = epkgs: [
      epkgs.dash
      epkgs.s
      prelude
    ];
  };

  dotted = buildEmacsPackage {
    pname = "dotted";
    version = "1.0";
    src = ./dotted.el;
    externaRequires = epkgs: [
      macros
    ];
  };

  fonts = buildEmacsPackage {
    pname = "fonts";
    version = "1.0";
    src = ./fonts.el;
    externalRequires = epkgs: [
      prelude
      cycle
      maybe
    ];
  };

  fs = buildEmacsPackage {
    pname = "fs";
    version = "1.0";
    src = ./fs.el;
    externalRequires = epkgs: [
      epkgs.dash
      epkgs.f
      epkgs.s
    ];
  };

  graph = buildEmacsPackage {
    pname = "graph";
    version = "1.0";
    src = ./graph.el;
    externalRequires = epkgs: [
      prelude
    ];
  };

  ivy-helpers = buildEmacsPackage {
    pname = "ivy-helpers";
    version = "1.0";
    src = ./ivy-helpers.el;
    externalRequires = epkgs: [
      tuple
      string
    ];
  };

  kbd = buildEmacsPackage {
    pname = "kbd";
    version = "1.0";
    src = ./kbd.el;
    externalRequires = epkgs: [
      al
      prelude
      set
      string
    ];
  };

  keyboard = buildEmacsPackage {
    pname = "keyboard";
    version = "1.0";
    src = ./keyboard.el;
    externalRequires = epkgs: [
      string
      number
    ];
  };

  laptop-battery = buildEmacsPackage {
    pname = "laptop-battery";
    version = "1.0";
    src = ./laptop-battery.el;
    externalRequires = epkgs: [
      epkgs.battery
      al
      maybe
    ];
  };

  list = buildEmacsPackage {
    pname = "list";
    version = "1.0";
    src = ./list.el;
    externalRequires = epkgs: [
      epkgs.dash
    ];
  };

  macros = buildEmacsPackage {
    pname = "macros";
    version = "1.0";
    src = ./macros.el;
    externalRequires = epkgs: [
      epkgs.f
      symbol
      string
    ];
  };

  math = buildEmacsPackage {
    pname = "math";
    version = "1.0";
    src = ./math.el;
    externalRequires = epkgs: [
      maybe
    ];
  };

  maybe = buildEmacsPackage {
    pname = "maybe";
    version = "1.0";
    src = ./maybe.el;
    externalRequires = epkgs: [
      list
    ];
  };

  number = buildEmacsPackage {
    pname = "number";
    version = "1.0";
    src = ./number.el;
    externalRequires = epkgs: [
      epkgs.dash
    ];
  };

  prelude = buildEmacsPackage {
    pname = "prelude";
    version = "1.0";
    src = ./prelude.el;
    externalRequires = epkgs: [
      epkgs.dash
      epkgs.f
      epkgs.s
      maybe
    ];
  };

  pulse-audio = buildEmacsPackage {
    pname = "pulse-audio";
    version = "1.0";
    src = ./pulse-audio.el;
    externalRequires = epkgs: [
      prelude
      string
    ];
  };

  random = buildEmacsPackage {
    pname = "random";
    version = "1.0";
    src = ./random.el;
    externalRequires = epkgs: [
      list
      math
      number
    ];
  };

  region = buildEmacsPackage {
    pname = "region";
    version = "1.0";
    src = ./region.el;
    externalRequires = epkgs: [ ];
  };

  screen-brightness = buildEmacsPackage {
    pname = "screen-brightness";
    version = "1.0";
    src = ./screen-brightness.el;
    externalRequires = epkgs: [
      prelude
    ];
  };

  scope = buildEmacsPackage {
    pname = "scope";
    version = "1.0";
    src = ./scope.el;
    externalRequires = epkgs: [
      al
      gt
      stack
      struct
    ];
  };

  scrot = buildEmacsPackage {
    pname = "scrot";
    version = "1.0";
    src = ./scrot.el;
    externalRequires = epkgs: [
      epkgs.f
      epkgs.ts
      string
      clipboard
    ];
  };

  series = buildEmacsPackage {
    pname = "series";
    version = "1.0";
    src = ./series.el;
    externalRequires = epkgs: [
      number
    ];
  };

  set = buildEmacsPackage {
    pname = "set";
    version = "1.0";
    src = ./set.el;
    externalRequires = epkgs: [
      epkgs.ht
      dotted
      struct
    ];
  };

  sequence = buildEmacsPackage {
    pname = "sequence";
    version = "1.0";
    src = ./sequence.el;
    externalRequires = epkgs: [
      epkgs.ht
      dotted
      struct
    ];
  };

  stack = buildEmacsPackage {
    pname = "stack";
    version = "1.0";
    src = ./stack.el;
    externalRequires = epkgs: [
      gt
      list
    ];
  };

  string = buildEmacsPackage {
    pname = "string";
    version = "1.0";
    src = ./string.el;
    externalRequires = epkgs: [
      epkgs.dash
      epkgs.s
    ];
  };

  struct = buildEmacsPackage {
    pname = "struct";
    version = "1.0";
    src = ./struct.el;
    externalRequires = epkgs: [
      epkgs.dash
      string
    ];
  };

  symbol = buildEmacsPackage {
    pname = "symbol";
    version = "1.0";
    src = ./symbol.el;
    externalRequires = epkgs: [
      string
    ];
  };

  timestring = buildEmacsPackage {
    pname = "tree";
    version = "1.0";
    src = ./tree.el;
    externalRequires = epkgs: [
      epkgs.ts
    ];
  };

  tree = buildEmacsPackage {
    pname = "tree";
    version = "1.0";
    src = ./tree.el;
    externalRequires = epkgs: [
      list
      maybe
      random
      set
      tuple
    ];
  };

  tuple = buildEmacsPackage {
    pname = "tuple";
    version = "1.0";
    src = ./tuple.el;
    externalRequires = epkgs: [ ];
  };

  vector = buildEmacsPackage {
    pname = "vector";
    version = "1.0";
    src = ./vector.el;
    externalRequires = epkgs: [
    ];
  };

  vterm-mgt = buildEmacsPackage {
    pname = "vterm-mgt";
    version = "1.0";
    src = ./window.el;
    externalRequires = epkgs: [
      epkgs.dash
      epkgs.vterm
      cycle
    ];
  };

  window = buildEmacsPackage {
    pname = "window";
    version = "1.0";
    src = ./window.el;
    externalRequires = epkgs: [
      maybe
    ];
  };

  window-manager = buildEmacsPackage {
    pname = "window-manager";
    version = "1.0";
    src = ./window-manager.el;
    externalRequires = epkgs: [
      epkgs.alert
      epkgs.dash
      epkgs.s
      epkgs.exwm
      maybe
      cycle
      kbd
    ];
  };

  zle = buildEmacsPackage {
    pname = "zle";
    version = "1.0";
    src = ./zle.el;
    externalRequires = epkgs: [
      maybe
    ];
  };
}
