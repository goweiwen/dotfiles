import XMonad
import XMonad.Util.Run
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

-- General config
myModMask = mod4Mask
myTerminal = "kitty tmux"
myLauncher = "albert"
myWorkspaces = [ "home", "www", "dev", "comm", "doc" ]

-- Appearance
myBackgroundColor = "#1c2023"
myForegroundColor = "#80c080"

myNormalBorderColor = myBackgroundColor
myFocusedBorderColor = myForegroundColor

myBorderWidth = 0

-- Hotkeys
myKeys =
  [ ("M-<Return>", spawn myTerminal)
  , ("M-<Space>", spawn "rofi -show run")

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
  ]

-- Status bar
myPP = def
  { ppCurrent = xmobarColor myBackgroundColor myForegroundColor . wrap " " " "
  , ppVisible = xmobarColor myForegroundColor "" . wrap " " " "
  , ppHidden = wrap " " " "
  , ppUrgent = xmobarColor "red" myBackgroundColor . wrap " " " "

  , ppTitle = wrap "<fn=1>" "</fn>"
  , ppTitleSanitize = xmobarStrip
  , ppLayout = \ _ -> ""

  , ppSep = " :: "
  , ppWsSep = " "
  }

myLogHook h = do
  dynamicLogWithPP myPP { ppOutput = hPutStrLn h }
  logHook desktopConfig

-- Startup hook
myStartupPrograms = ["compton", "dunst", "firefox", myTerminal]
myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  mapM_ spawnOnce myStartupPrograms

-- Hooks
myManageHook = composeAll
  [ className =? "Firefox" --> doShift "www"
  ]

myLayoutHook = smartBorders . smartSpacingWithEdge 5 $ layoutHook desktopConfig

-- Status Bar
main = do
  h <- spawnPipe "xmobar"
  xmonad $ desktopConfig
    { modMask = myModMask
      , terminal = myTerminal
      , workspaces = myWorkspaces

    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = myBorderWidth

    , startupHook = startupHook desktopConfig <+> myStartupHook
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = myLayoutHook
    , logHook = myLogHook h
    }
    `additionalKeysP` myKeys
