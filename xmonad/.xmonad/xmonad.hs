import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.StackSet (RationalRect (..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

-- General config
myModMask = mod4Mask
myTerminalTmux = "termite -e tmux"
myTerminal = "termite"
myScratchpad = "termite --name scratchpad -e tmux"
myLauncher = "rofi -show combi -combi-modi 'window,drun,run,ssh'"
myWallpaper = "feh --bg-fill \"$HOME/Pictures/Wallpapers/$(ls $HOME/Pictures/Wallpapers | sort -R | head -n 1)\""
myWorkspaces = [ "home", "www", "dev", "comm", "doc" ]

-- Appearance
myBackgroundColor = "#1c2023"
myForegroundColor = "#99cc99"

myNormalBorderColor = myBackgroundColor
myFocusedBorderColor = myForegroundColor

myBorderWidth = 1

-- Hotkeys
myKeys =
  [ ("M-<Return>", spawn myTerminalTmux)
  , ("M-S-<Return>", spawn myTerminal)
  , ("M-<Space>", spawn myLauncher)
  , ("M-`", scratchPad)
  , ("M-n", spawn myWallpaper)

  -- Browser buttons
  , ("M-<F1>", spawn "xdotool key XF86_Back")
  , ("M-<F2>", spawn "xdotool key XF86_Forward")
  , ("M-<F3>", spawn "xdotool key XF86_Refresh")

  -- Brightness control
  , ("M-<F6>", spawn "light -U 5")
  , ("M-<F7>", spawn "light -A 5")

  -- Volume control
  , ("M-<F8>", spawn "pactl set-sink-mute 0 toggle")
  , ("M-<F9>", spawn "pactl set-sink-volume 0 -1024")
  , ("M-<F10>", spawn "pactl set-sink-volume 0 +1024")
  ] ++ myWorkspaceKeys

myWorkspaceKeys =
  concat $ map workspaceKeys workspaces
    where
      workspaces = zip myWorkspaces $ map show [1..9]
      workspaceKeys = \ (i, k) ->
        [ ("C-" ++ k, windows $ W.greedyView i)
        , ("C-S-" ++ k, windows $ W.greedyView i . W.shift i)
        ]

myWorkspaceKeys' =
  [ (m ++ k, windows $ f i)
  | (i, k) <- zip myWorkspaces $ map show [1..9]
  , (f, m) <-
    [ (W.greedyView, "C-")
    , (\ w -> W.greedyView w . W.shift w, "C-S-")
    ]
  ]

-- Startup hook
myStartupPrograms =
  [ "polybar default"
  , myWallpaper
  ]

myStartupHook :: X ()
myStartupHook = do
  mapM_ spawnOnce myStartupPrograms

-- Scratchpad
manageScratchPad = scratchpadManageHook $ RationalRect x y w h
  where
    h = 1
    w = 1/3
    x = 1-w
    y = 0
scratchPad = scratchpadSpawnActionCustom myScratchpad

-- Hooks
myManageHook = composeAll
  [ className =? "Firefox" --> doShift "www"
  , className =? "qutebrowser" --> doShift "www"
  ] <+> manageScratchPad

myLayoutHook = smartBorders $ layoutHook desktopConfig

-- Main
main = xmonad $ desktopConfig
    { modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces

    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = myBorderWidth

    , startupHook = startupHook desktopConfig <+> myStartupHook
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = myLayoutHook
    , logHook = logHook desktopConfig
        <+> ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace
    }
    `additionalKeysP` myKeys
