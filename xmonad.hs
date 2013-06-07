import System.IO

import Control.Concurrent
import Control.Monad

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.String.Utils

-- TODO: remove after moving JSON to Tianbar
import DBus
import XMonad.Util.NamedWindows
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String (renderMarkup)

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

workspaceIcon :: String -> Maybe String
workspaceIcon s | s == gitWorkspace  = Just "code-fork"
                | s == mailWorkspace = Just "envelope"
                | s == imWorkspace   = Just "comment-alt"
                | otherwise          = Nothing

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

blazeLog :: Client -> X ()
blazeLog client = do

    winset <- gets windowset
    urgents <- readUrgents
    let ld = description . S.layout . S.workspace . S.current $ winset
    sort_ <- mkWsSort getWsCompare
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    let html = renderMarkup $ tianbarHtml ld wt sort_ urgents winset
    liftIO $ emit client sig { signalBody = [ toVariant html ] }

tianbarHtml :: String        -- ^ layout description
            -> String        -- ^ window title
            -> WorkspaceSort -- ^ workspace sort (?)
            -> [Window]      -- ^ urgent windows
            -> WindowSet     -- ^ all windows
            -> Markup
tianbarHtml layout title sort_ urgents windows = do
    H.span ! A.class_ (toValue "workspaces") $
        mapM_ wsHtml $ sort_ ws
    H.span ! A.class_ (toValue "layout") $ toMarkup layout
    H.span ! A.class_ (toValue "title") $ toMarkup title
    where
        ws = map S.workspace (S.current windows : S.visible windows) ++ S.hidden windows
        this     = S.currentTag windows
        visibles = map (S.tag . S.workspace) (S.visible windows)

        wsHtml w = H.span ! A.class_ (toValue $ unwords classes) $
            if isJust icon
                then do
                    H.i ! A.class_ (toValue $ "icon-" ++ fromJust icon) $
                        toMarkup ""
                    H.sub $ toMarkup tag
                else
                    toMarkup tag
            where
                classes =
                    ["workspace"] ++
                    ["current" | tag == this] ++
                    ["hidden"  | tag `notElem` visibles] ++
                    ["urgent"  | isUrgent] ++
                    ["empty"   | noWindows]
                tag = S.tag w
                icon = workspaceIcon tag
                isUrgent = any (\x -> maybe False (== tag) (S.findTag x windows)) urgents
                noWindows = isNothing (S.stack w)

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
        , logHook = blazeLog client
        , modMask = modm
        } `removeKeys`
        [ (modm                 , xK_p)
        , (modm                 , xK_Return)
        ] `additionalKeys` keys
