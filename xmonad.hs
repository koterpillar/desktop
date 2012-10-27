import System.IO

import Control.Monad

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils

import DBus.Client.Simple

import XMonad
import XMonad.Actions.Search
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ComboP
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeys, removeKeys)

import System.Gnome.GConf

import Graphics.X11.ExtraTypes.XF86

import System.Taffybar.XMonadLog

-- For default configuration, see
-- http://xmonad.org/xmonad-docs/xmonad/src/XMonad-Config.html

theme :: Theme
theme = defaultTheme { activeColor = "#FFE8C9"
                     , activeTextColor = "#000000"
                     , activeBorderColor = "#FFE8C9"
                     , urgentColor = "#FF0000"
                     , urgentTextColor = "#FFFFFF"
                     , urgentBorderColor = "#FFFFFF"
                     , fontName = "xft:ubuntu:size=9"
                     }

floatLayout = simpleFloat' shrinkText theme

tabbedLayout = tabbed shrinkText theme

imLayout = named "IM" $
    combineTwoP (TwoPane 0.03 0.2) rosterLayout mainLayout isRoster
    where rosterLayout    = smartBorders mosaicLayout
          mainLayout      = mosaicLayout
          isRoster        = pidginRoster `Or` skypeRoster
          pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
          -- TODO: distinguish Skype's main window better
          skypeRoster     = Or (Title $ skypeLogin ++ " - Skype™") (Title "Skype™ 4.0 для Linux")
          skypeLogin      = "koterpillar"

mosaicLayout = MosaicAlt M.empty

layout = onWorkspace "IM7" imLayout $
        named "Mosaic" (smartBorders mosaicLayout)
    ||| named "Tabs" (smartBorders tabbedLayout)
    ||| named "Float" (smartBorders floatLayout)

-- For named workspaces, make the number a subscript
subNumber :: String -> String
subNumber [x] = [x]
subNumber named = taffybarEscape (take lname named) ++ wrap smallFont "</span>" (drop lname named)
    where lname = (length named) - 1
          smallFont = "<span font_desc=\"7\">"

taffybarColorUnsafe :: String -> String -> String -> String
taffybarColorUnsafe fg bg = wrap t "</span>"
    where t = concat ["<span fgcolor=\"", fg, if null bg then "" else "\" bgcolor=\"" ++ bg , "\">"]

pp :: PP
pp = defaultPP { ppTitle = wrap "<b>" "</b>" . taffybarEscape . shorten 150
               , ppCurrent = taffybarColorUnsafe "black" "#ffc060" . pad . subNumber
               , ppVisible = taffybarColorUnsafe "black" "#f0f0f0" . pad . subNumber
               , ppUrgent = taffybarColorUnsafe "white" "red" . pad . subNumber
               , ppHidden = taffybarColorUnsafe "black" "" . subNumber
               , ppHiddenNoWindows = taffybarColorUnsafe "#A0A0A0" "" . subNumber
               , ppOrder = spaceBefore
               }
     where
        spaceBefore [ws, l, wt] = [' ':ws, l, wt]

wsName :: Int -> String
wsName x = maybeName x ++ show x
    where maybeName 4 = "Git"
          maybeName 6 = "Mail"
          maybeName 7 = "IM"
          maybeName _ = ""

myWorkspaces = map wsName [1..9 :: Int]

myManageHook = composeAll
    [ className =? "Gitg" --> doShift (wsName 4)
    , className =? "Thunderbird" --> doShift (wsName 6)
    , className =? "Pidgin" <||> className =? "Skype" --> doShift (wsName 7)
    ]

doShutdown = consoleKit "Stop"
doReboot = consoleKit "Restart"

consoleKit :: String -> X ()
consoleKit x = spawn $
    "dbus-send --system --print-reply"
    ++ " --dest=org.freedesktop.ConsoleKit"
    ++ " /org/freedesktop/ConsoleKit/Manager"
    ++ " org.freedesktop.ConsoleKit.Manager." ++ x

stringMenu :: [(String, X a)] -> X a
stringMenu items = do
    action <- dmenu $ map fst items
    snd . fromJust $ find (((==) action) . fst) items

shutdownMenu :: X ()
shutdownMenu = stringMenu [ ("shutdown", doShutdown)
                          , ("reboot", doReboot)
                          ]

getUrlHandler :: GConf -> String -> IO String
getUrlHandler gconf scheme = do
    handler <- gconf `gconfGet` ("/desktop/gnome/url-handlers/" ++ scheme ++ "/command")
    return $ replace " %u" "" $ replace " %s" "" $ handler

modm = mod4Mask

main = do
    client <- connectSession
    gconf <- gconfGetDefault
    browser <- getUrlHandler gconf "http"
    email <- getUrlHandler gconf "mailto"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal = "x-terminal-emulator"
        , workspaces = myWorkspaces
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layout
        , logHook = dbusLogWithPP client pp
        , modMask = modm
        } `removeKeys`
        [ (modm                 , xK_p)
        , (modm                 , xK_Return)
        ] `additionalKeys`
        [ ((0                   , xF86XK_PowerOff  ), shutdownMenu)
        , ((0                   , xF86XK_HomePage  ), spawn browser)
        , ((0                   , xF86XK_Mail      ), spawn email)
        , ((0                   , xF86XK_Messenger ), spawn "pidgin")
        , ((modm                , xK_b    ), sendMessage ToggleStruts)
        , ((modm                , xK_s    ), selectSearch google)

        , ((modm                , xK_h    ), withFocused $ sendMessage . expandWindowAlt)
        , ((modm                , xK_l    ), withFocused $ sendMessage . shrinkWindowAlt)
        , ((modm .|. shiftMask  , xK_h    ), withFocused $ sendMessage . tallWindowAlt)
        , ((modm .|. shiftMask  , xK_l    ), withFocused $ sendMessage . wideWindowAlt)
        , ((modm .|. controlMask, xK_space), sendMessage resetAlt)
        ]
