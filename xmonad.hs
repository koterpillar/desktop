import System.IO

import DBus.Client.Simple

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)

import System.Taffybar.XMonadLog (dbusLogWithPP)

layout = Full ||| tiled ||| Mirror tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- https://github.com/travitch/taffybar/issues/18
escapeAmp :: String -> String
escapeAmp = concat . map escape
            where escape '&' = "&amp;"
                  escape c = [c]

main = do
    client <- connectSession
    let pp = defaultPP { ppTitle = escapeAmp . shorten 150
                       }
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layout
        , logHook = dbusLogWithPP client pp
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ --((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        --, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        --, ((0, xK_Print), spawn "scrot")
        ]

