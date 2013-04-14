import System.IO

import Control.Concurrent
import Control.Monad

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils

import DBus.Client.Simple

import XMonad
import XMonad.Actions.Search
import qualified XMonad.Actions.Volume as Volume
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
          skypeRoster     = Title $ skypeLogin ++ " - Skype™"
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

namedWorkspaces = [ ("4", "Git")
                  , ("6", "Mail")
                  , ("7", "IM")
                  ]

wsName :: (String, String) -> String
wsName = uncurry $ flip (++)

wsFromIndex :: String -> String
wsFromIndex n = case find ((==) n . fst) namedWorkspaces of
    Just ws -> wsName ws
    Nothing -> n

wsFromName :: String -> String
wsFromName n = case find ((==) n . snd) namedWorkspaces of
    Just ws -> wsName ws
    Nothing -> n

myWorkspaces = map wsFromIndex $ map show [1..9] ++ ["0", "-", "="]

myManageHook = composeAll
    [ className =? "Gitg" --> doShift (wsFromName "Git")
    , className =? "Thunderbird" --> doShift (wsFromName "Mail")
    , className =? "Pidgin" <||> className =? "Skype" --> doShift (wsFromName "IM")
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

type OSDOptions = M.Map String String

optionsString :: OSDOptions -> String
optionsString = intercalate " " . map (uncurry makeOption) . M.toList
    where makeOption k v = "--" ++ k ++ "=" ++ v

spawnOsd :: MonadIO m => OSDOptions -> m ()
spawnOsd opts = spawn $ "osd_cat " ++ optionsString opts

osdPercentage :: MonadIO m => OSDOptions -> Double -> m ()
osdPercentage opts percent = do
    let opts' = M.insert "barmode" "percentage" opts
    spawnOsd $ M.union underlay opts'
    liftIO $ threadDelay 50000
    spawnOsd $ M.insert "percentage" (show $ (truncate percent :: Integer)) opts'

osdOpts :: OSDOptions
osdOpts = M.fromList [ ("align", "center")
                     , ("pos", "middle")
                     , ("delay", "1")
                     , ("color", "darkgreen")
                     ]

underlay :: OSDOptions
underlay = M.fromList [ ("color", "white")
                      , ("percentage", "100")
                      , ("outline", "2")
                      , ("outlinecolour", "white")
                      ]

volumeStep = 3
lowerVolume = Volume.lowerVolume volumeStep >>= osdPercentage osdOpts
raiseVolume = Volume.raiseVolume volumeStep >>= osdPercentage osdOpts
toggleMute  = void Volume.toggleMute

modm = mod4Mask

main = do
    client <- connectSession
    gconf <- gconfGetDefault
    browser <- getUrlHandler gconf "http"
    email <- getUrlHandler gconf "mailto"
    let keys = [ ((0                   , xF86XK_PowerOff ), shutdownMenu)
               , ((0                   , xF86XK_HomePage ), spawn browser)
               , ((0                   , xF86XK_Mail     ), spawn email)
               , ((0                   , xF86XK_Messenger), spawn "pidgin")

               , ((0                   , xF86XK_AudioMute), toggleMute)
               , ((0                   , xF86XK_AudioLowerVolume), lowerVolume)
               , ((0                   , xF86XK_AudioRaiseVolume), raiseVolume)

               , ((modm                , xK_b    ), sendMessage ToggleStruts)
               , ((modm                , xK_s    ), selectSearch google)

               , ((modm                , xK_p    ), spawn "synapse")

               , ((modm                , xK_h    ), withFocused $ sendMessage . expandWindowAlt)
               , ((modm                , xK_l    ), withFocused $ sendMessage . shrinkWindowAlt)
               , ((modm .|. shiftMask  , xK_h    ), withFocused $ sendMessage . tallWindowAlt)
               , ((modm .|. shiftMask  , xK_l    ), withFocused $ sendMessage . wideWindowAlt)
               , ((modm .|. controlMask, xK_space), sendMessage resetAlt)
               ]
               ++
               -- Switch/move windows to workspaces
               [((m .|. modm, k), windows $ f i)
                   | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]
                   , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
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
        ] `additionalKeys` keys
