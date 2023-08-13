module Main where

import Data.Function ((&))
import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.Decoration
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers (..))
import XMonad.Layout.Tabbed (TabbedDecoration)
import XMonad.Layout.Tabbed qualified as Tabbed
import XMonad.StackSet qualified as StackSet
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, removeKeysP)

data Mode = Normal | Presentation

main :: IO ()
main = xmonad . ewmh $ myConfig

myConfig ::
  XConfig
    ( MultiToggle
        ( HCons
            StdTransformers
            XMonad.Layout.MultiToggle.EOT
        )
        ( ModifiedLayout
            ( Decoration
                TabbedDecoration
                DefaultShrinker
            )
            Tall
        )
    )
myConfig =
  conf
    { modMask = modKey,
      terminal = term Normal,
      focusedBorderColor = "#859900",
      layoutHook = layout,
      startupHook = setDefaultCursor xC_heart,
      workspaces = workspaceNames
    }
    `additionalKeysP` ( [
                          -- fullscreen
                          ("M-e", sendMessage $ Toggle NBFULL),
                          -- i3-like keybindings, because I’m spoiled
                          ("M-S-x", kill),
                          -- exchange M-Ret and M-S-Ret
                          ("M-<Return>", spawn $ term Normal),
                          ("C-M-<Return>", spawn $ term Presentation),
                          ("M-S-<Return>", windows StackSet.swapMaster)
                          -- open simple exec dmenu
                        ]
                          ++
                          -- something something workspaces
                          [ (otherModMasks ++ "M-" ++ [key], action tag)
                            | (tag, key) <- zip workspaceNames "123456789",
                              (otherModMasks, action) <-
                                [ ("", windows . StackSet.greedyView),
                                  ("S-", windows . StackSet.shift)
                                ]
                          ]
                          ++
                          -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
                          -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
                          [ ("M-v", focusToScreen 0),
                            -- , ("M-l", focusToScreen 1)
                            ("M-c", focusToScreen 2),
                            ("M-S-v", windowToScreen 0),
                            ("M-S-l", windowToScreen 1),
                            ("M-S-c", windowToScreen 2)
                          ]
                          -- ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
                          --   | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
                          --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
                      )
    `additionalKeys`
    -- arrow keys should move as well (hjkl blindness)
    [ ((modKey, xK_Up), windows StackSet.focusUp),
      ((modKey, xK_Down), windows StackSet.focusDown)
    ]
    `removeKeysP` [
                    -- previous kill command
                    "M-S-c",
                    -- It is way to easy to kill everything by default
                    "M-S-q",
                    -- no idea, I want to use it for Mozc
                    "M-n"
                  ]
  where
    conf = def
    workspaceNames = conf & workspaces
    modKey = mod4Mask
    -- TODO: meh
    term :: Mode -> String
    -- TODO: get terminal-emulator from the system config (currently alacritty)
    term Normal = "terminal-emulator"
    term Presentation = "notify-send TODO: currently not terminal presentation mode implemented" -- "terminal- -u ~/.config/lilyterm/pres.conf"
    toScreen with _number = screenWorkspace 0 >>= \ws -> whenJust ws (windows . with)
    focusToScreen = toScreen StackSet.view
    windowToScreen = toScreen StackSet.shift

-- copied from Xmonad.Config
layout ::
  MultiToggle
    (HCons StdTransformers EOT)
    (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Tall)
    Window
layout =
  tiled
    & Tabbed.addTabsBottom Tabbed.shrinkText def
    & toggleFullscreen
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100
    toggleFullscreen = mkToggle1 NBFULL
