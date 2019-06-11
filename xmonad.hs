import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
-- HOW TO HOOK THIS!?
import XMonad.Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
--- Layouts
-- Resizable tile layout
import XMonad.Layout.ResizableTile
-- Simple two pane layout.
import XMonad.Layout.TwoPane
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Dwindle

import System.IO
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import XMonad.Prompt.ConfirmPrompt
import qualified XMonad.Prompt as XP


myTabConfig = def { activeColor = "#2F3D44"
                  , inactiveColor = "#556064"
                  , urgentColor = "#FDF6E3"
                  , activeBorderColor = "#454948"
                  , inactiveBorderColor = "#454948"
                  , urgentBorderColor = "#268BD2"
                  , activeTextColor = "#80FFF9"
                  , inactiveTextColor = "#1ABC9C"
                  , urgentTextColor = "#1ABC9C"
                  }

myPP i = dynamicLogWithPP defaultPP
           {ppTitle = xmobarColor "green" "" . shorten 50
           , ppOutput  = hPutStrLn i
}


myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]


--xmobarEscape = concatMap doubleLts
--  where doubleLts '<' = "<<"
--        doubleLts x   = [x]
--
--myWorkspaces :: [String]
--myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5","6","7","8","9"]
--
--  where
--         clickable l = [ "<action=xdotool key alt+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
--                             (i,ws) <- zip [1..5] l,
--                            let n = i ]

myLayout = avoidStruts $
  tiled
  ||| Mirror tiled
  ||| noBorders (tabbed shrinkText myTabConfig)
  ||| noBorders Full
--  ||| twopane
--  ||| Mirror twopane
--  ||| emptyBSP
--  ||| Spiral L XMonad.Layout.Dwindle.CW (3/2) (11/10) -- L means the non-main windows are put to the left.

  where
     -- The last parameter is fraction to multiply the slave window heights
     -- with. Useless here.
     tiled = spacing 3 $ ResizableTall nmaster delta ratio []
     -- In this layout the second pane will only show the focused window.
     twopane = spacing 3 $ TwoPane delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                        <+> manageHook defaultConfig
        , handleEventHook = handleEventHook def <+> docksEventHook
--        , workspaces = myWorkspaces
        , layoutHook = myLayout
        , logHook = myPP xmproc
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys` myKeys
--         `removeKeys`
--        [ (mod4Mask .|. shiftMask, xK_q)
--        ]

myKeys = [
    -- utilities
    ((mod4Mask .|. shiftMask, xK_z), spawn "slock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    -- audio control
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    -- windows control
    , ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorExpand)
    -- section control
    , ((mod4Mask .|. shiftMask, xK_q), confirmPrompt XP.defaultXPConfig "exit" $ io (exitWith ExitSuccess))
    , ((mod4Mask .|. shiftMask, xK_s), confirmPrompt XP.defaultXPConfig "suspend" $ spawn "systemctl suspend")
    , ((mod4Mask .|. shiftMask, xK_b), confirmPrompt XP.defaultXPConfig "use only laptop screen" $ spawn "~/.screenlayout/only-laptop.sh")
    , ((mod4Mask .|. shiftMask, xK_n), confirmPrompt XP.defaultXPConfig "use the additional screen setup 1" $ spawn "~/.screenlayout/home-two-screens.sh")
    , ((mod4Mask .|. shiftMask, xK_m), confirmPrompt XP.defaultXPConfig "use the additional screen setup 2" $ spawn "~/.screenlayout/work-two-screens.sh")
    ]
