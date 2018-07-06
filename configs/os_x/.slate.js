// Support flag to branch KBDs depending on presence of Ergodox keyboard.

// Since the Ergodox has complicated modifier keys like "hyper" and "meh" key,
// we should prefer to use these when that keyboard is attached because it
// reduces the potential for collisions for Emacs KBDs. This becomes
// problematic, however, when the Ergodox is not attached because these keys are
// unavailable. Slate KBDs. Under these circumstances, potential collisions
// with Emacs KBDs is acceptable.

var ergodox_attached = false;

var HYPER = ":alt;shift;cmd;ctrl";
var MEH = ":alt;shift;ctrl";

var modal_key = ergodox_attached ? HYPER : ":ctrl;shift";
var resize_key = ergodox_attached ? MEH : ":alt;shift";

// Configs
S.cfga({
  defaultToCurrentScreen: true,
  secondsBetweenRepeat: 0.1,
  checkDefaultsOnLoad: true,
  focusCheckWidthMax: 3000
});

// window resizing bindings
var window_resizing_bindings = {
  ";": {
    x: "screenSizeX/3*2 + screenOriginX+20",
    y: "screenOriginY+20",
    width: "screenSizeX/3 - 40",
    height: "screenSizeY-100"
  },
  g: {
    x: "screenOriginX+20",
    y: "screenOriginY+20",
    width: "screenSizeX/3*2 - 40",
    height: "screenSizeY-100"
  },
  o: {
    x: "screenSizeX / 2 + screenOriginX + 20",
    y: "screenOriginY + 20",
    width: "screenSizeX / 2 - 40",
    height: "(screenSizeY - 120) / 2"
  },
  ",": {
    x: "screenSizeX / 2 + screenOriginX + 20",
    y: "(screenSizeY - 120) / 2 + 20 + 20",
    width: "screenSizeX / 2 - 40",
    height: "(screenSizeY - 120) / 2"
  },
  h: {
    x: "screenOriginX+20",
    y: "screenOriginY+20",
    width: "screenSizeX*0.5 - 40",
    height: "screenSizeY-100"
  },
  j: {
    x: "screenOriginX+screenSizeX/6",
    y: "screenOriginY+20",
    width: "2*screenSizeX/3",
    height: "screenSizeY - 100"
  },
  k: {
    x: "screenOriginX+20",
    y: "screenOriginY+20",
    width: "screenSizeX - 40",
    height: "screenSizeY - 100"
  },
  l: {
    x: "screenSizeX/2 + screenOriginX+20",
    y: "screenOriginY+20",
    width: "screenSizeX*0.5 - 40",
    height: "screenSizeY-100"
  }
};

var window_resizing_bindings = Object.keys(window_resizing_bindings).reduce(
  function(acc, kbd) {
    acc[kbd + resize_key] = S.op("move", window_resizing_bindings[kbd]);
    return acc;
  },
  {}
);

S.bnda(window_resizing_bindings);

// Moves applications across multiple screens
var throwLeft = slate.operation("throw", {
  screen: "0",
  width: "screenSizeX",
  height: "screenSizeY"
});
var throwRight = slate.operation("throw", {
  screen: "1",
  width: "screenSizeX",
  height: "screenSizeY"
});

slate.bind("1:ctrl", throwLeft);
slate.bind("2:ctrl", throwRight);

var focus_apps = {
  1: "1Password",
  i: "iTunes",
  a: "Atom",
  h: "Dash",
  e: "Emacs",
  t: "iTerm2",
  m: "Messages",
  s: "Spotify",
  c: "Google Chrome",
  l: "LimeChat",
  k: "Slack",
  w: "Wireshark",
  p: "Tomato One",
  d: "Discord"
};

Object.keys(focus_apps).forEach(function(key) {
  app = focus_apps[key];
  S.bind(key + modal_key, S.op("focus", { app: app }));
});
