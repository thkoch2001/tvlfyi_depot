/* globals S,slate,_ */
// Bindings for slate.
// Forked from rschmukler/dotfiles
// Hotkeys for quickly opening apps & changing window size


var modal_key = ":alt";

// Configs
S.cfga({
  "defaultToCurrentScreen" : true,
  "secondsBetweenRepeat" : 0.1,
  "checkDefaultsOnLoad" : true,
  "focusCheckWidthMax" : 3000
});

// Window size /position shortcuts
// ctrl+shift+h = use left half of screen.
// ctrl+shift+l = right half, hjkl
// ctrl+shift+m = use full window

S.bnda({
  // Push Bindings
  "l:ctrl;shift" : S.op("move", { "x": "screenSizeX/2 + screenOriginX+20", "y": "screenOriginY+20", "width": "screenSizeX*0.5 - 40", "height": "screenSizeY-100" }),
  ";:ctrl;shift" : S.op("move", { "x": "screenSizeX/3*2 + screenOriginX+20", "y": "screenOriginY+20", "width": "screenSizeX/3 - 40", "height": "screenSizeY-100" }),

  "h:ctrl;shift" : S.op("move", { "x": "screenOriginX+20", "y": "screenOriginY+20", "width": "screenSizeX*0.5 - 40", "height": "screenSizeY-100" }),
  "g:ctrl;shift" : S.op("move", { "x": "screenOriginX+20", "y": "screenOriginY+20", "width": "screenSizeX/3*2 - 40", "height": "screenSizeY-100" }),

  "k:ctrl;shift" : S.op("move", { "x": "screenOriginX+20", "y": "screenOriginY+20", "width": "screenSizeX - 40", "height": "screenSizeY/2 - 20" }),
  "j:ctrl;shift" : S.op("move", { "x": "screenOriginX+20", "y": "screenSizeY/2 + screenOriginY+20", "width": "screenSizeX - 40", "height": "screenSizeY/2 - 40" }),
  "m:ctrl;shift" : S.op("move", { "x": "screenOriginX+20", "y": "screenOriginY+20", "width": "screenSizeX - 40", "height": "screenSizeY - 100" }),
  "n:ctrl;shift" : S.op("move", { "x": "screenOriginX+screenSizeX/6", "y": "screenOriginY+20", "width": "2*screenSizeX/3", "height": "screenSizeY - 100" }),
  "b:ctrl;shift" : S.op("move", { "x": "screenOriginX+screenSizeX/4", "y": "screenOriginY+20", "width": "screenSizeX/2", "height": "screenSizeY - 100" }),
});


// Moves applications across multiple screens
// S.bnda('1:ctrl,alt', S.op('throw', { screen: '0', style: 'resize' });
// S.bnda('2:ctrl,alt', S.op('throw', { screen: '1', style: 'resize' });


// Binds modal key + {char} to focus different open apps
// alt + d = focus Dash

var focus_apps = {
  a: 'Atom',
  h: 'Dash',
  e: 'Emacs',
  t: 'iTerm2',
  m: 'Messages',
  s: 'Spotify',
  c: 'Google Chrome',
  l: 'LimeChat',
  k: 'Slack',
  w: 'Wireshark',
};


Object.keys(focus_apps).forEach(function(key) {
  app = focus_apps[key];
  S.bind(key + modal_key, S.op("focus", {app: app}));
});
