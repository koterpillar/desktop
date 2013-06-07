import System.IO

import Control.Concurrent
import Control.Monad

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils

import DBus -- TODO: remove after moving JSON to Tianbar
import Text.JSON -- TODO: remove after moving JSON to Tianbar
import XMonad.Util.NamedWindows -- TODO: remove after moving JSON to Tianbar

import DBus.Client

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
import qualified XMonad.StackSet as S
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Util.WorkspaceCompare

import System.Gnome.GConf

import Graphics.X11.ExtraTypes.XF86

import System.Tianbar.XMonadLog

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

gitWorkspace  = "4"
mailWorkspace = "6"
imWorkspace   = "7"

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

layout = onWorkspace imWorkspace imLayout $
        named "Mosaic" (smartBorders mosaicLayout)
    ||| named "Tabs" (smartBorders tabbedLayout)
    ||| named "Float" (smartBorders floatLayout)

myWorkspaces = map show [1..9] ++ ["0", "-", "="]

myManageHook = composeAll
    [ className =? "Gitg" --> doShift gitWorkspace
    , className =? "Thunderbird" --> doShift mailWorkspace
    , className =? "Pidgin" <||> className =? "Skype" --> doShift imWorkspace
    ]

getUrlHandler :: GConf -> String -> IO String
getUrlHandler gconf scheme = do
    handler <- gconf `gconfGet` ("/desktop/gnome/url-handlers/" ++ scheme ++ "/command")
    return $ replace " %u" "" $ replace " %s" "" handler

modm = mod4Mask

sig :: Signal
sig = signal (fromJust $ parseObjectPath "/org/xmonad/Log")
              (fromJust $ parseInterfaceName "org.xmonad.Log")
              (fromJust $ parseMemberName "Update")

jsString = JSString . toJSString
jsObject = JSObject . toJSObject

jsonLog :: Client -> X ()
jsonLog client = do

    winset <- gets windowset
    urgents <- readUrgents

    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset

    sort_ <- mkWsSort getWsCompare

    -- workspace list
    let ws = workspacesJson sort_ urgents winset

    -- window title
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    let json = JSObject $ toJSObject [ ("layout", jsString ld)
                                     , ("workspaces", ws)
                                     , ("title", jsString wt)
                                     ]
    let jsonStr = showJSValue json ""
    liftIO $ emit client sig { signalBody = [ toVariant jsonStr ] }
    -- liftIO $ putStrLn jsonStr

workspacesJson :: WorkspaceSort -> [Window] -> WindowSet -> JSValue
workspacesJson sort_ urgents s = JSArray $ map wsJson $ sort_ ws
   where ws = map S.workspace (S.current s : S.visible s) ++ S.hidden s
         this     = S.currentTag s
         visibles = map (S.tag . S.workspace) (S.visible s)

         wsJson w = jsObject [ ("tag",     jsString $ tag)
                             , ("current", JSBool $ tag == this)
                             , ("visible", JSBool $ tag `elem` visibles)
                             , ("urgent",  JSBool $ isUrgent)
                             , ("windows", JSBool $ hasWindows)
                             ]
          where tag = S.tag w
                isUrgent = any (\x -> maybe False (== tag) (S.findTag x s)) urgents
                hasWindows = isJust (S.stack w)

confirmShutdown = "/usr/lib/indicator-session/gtk-logout-helper -s"

main = do
    client <- connectSession
    gconf <- gconfGetDefault
    browser <- getUrlHandler gconf "http"
    email <- getUrlHandler gconf "mailto"
    let keys = [ ((0                   , xF86XK_PowerOff ), spawn confirmShutdown)
               , ((0                   , xF86XK_HomePage ), spawn browser)
               , ((0                   , xF86XK_Mail     ), spawn email)
               , ((0                   , xF86XK_Messenger), spawn "pidgin")

               , ((modm                , xK_b    ), sendMessage ToggleStruts)
               , ((modm                , xK_s    ), selectSearchBrowser browser google)

               , ((modm                , xK_o    ), spawn "synapse")

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
                   , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]]
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal = "x-terminal-emulator"
        , workspaces = myWorkspaces
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts layout
        , logHook = jsonLog client
        , modMask = modm
        } `removeKeys`
        [ (modm                 , xK_p)
        , (modm                 , xK_Return)
        ] `additionalKeys` keys
