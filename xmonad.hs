import System.IO

import DBus.Client.Simple

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)

import Graphics.X11.ExtraTypes.XF86

import System.Taffybar.XMonadLog (dbusLogWithPP, taffybarEscape)

layout = noBorders Full ||| tiled ||| Mirror tiled ||| simpleFloat
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- For default configuration, see
-- http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Config.html

main = do
    client <- connectSession
    let pp = defaultPP { ppTitle = taffybarEscape . shorten 150
                       , ppCurrent = taffybarEscape . ppCurrent defaultPP
                       , ppVisible = taffybarEscape . ppVisible defaultPP
                       }
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layout
        , logHook = dbusLogWithPP client pp
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod1Mask,               xK_Tab   ), windows W.focusDown) -- Alt-Tab to switch windows
        , ((mod1Mask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- Alt-Shift-Tab
        , ((0                     , xF86XK_PowerOff), spawn "gnome-session-quit --power-off")
        , ((mod4Mask              , xK_Return), spawn $ XMonad.terminal defaultConfig)
        , ((0                     , xF86XK_HomePage), spawn "x-www-browser" )
        --, ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        --, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        --, ((0, xK_Print), spawn "scrot")
        ]

