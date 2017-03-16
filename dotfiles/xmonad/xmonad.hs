import Control.Monad

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid

import DBus.Client

import Graphics.X11.Xrandr

import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import XMonad

import XMonad.Config.Desktop

import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Search

import XMonad.Hooks.UrgencyHook

import XMonad.Layout.ComboP
import XMonad.Layout.Fullscreen
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import qualified XMonad.StackSet as S

import XMonad.Util.EZConfig
import XMonad.Util.Run

import System.Environment
import System.Tianbar.XMonadLog

-- For configuration documentation, see
-- http://xmonad.org/xmonad-docs/xmonad-contrib/index.html
theme :: Theme
theme =
  def
  { activeColor = "#FFE8C9"
  , activeTextColor = "#000000"
  , activeBorderColor = "#FFE8C9"
  , urgentColor = "#FF0000"
  , urgentTextColor = "#FFFFFF"
  , urgentBorderColor = "#FFFFFF"
  , fontName = "-*-fixed-medium-*-*-*-14-*-*-*-*-*-*-*"
  }

floatLayout = simpleFloat' shrinkText theme

tabbedLayout = tabbed shrinkText theme

mailWorkspace = "6"

imWorkspace = "7"

workspaceIcon :: String -> Maybe String
workspaceIcon s
  | s == mailWorkspace = Just "envelope-o"
  | s == imWorkspace = Just "comment-o"
  | otherwise = Nothing

imLayout =
  named "IM" $ combineTwoP (TwoPane 0.03 0.2) rosterLayout mainLayout isRoster
  where
    rosterLayout = smartBorders mosaicLayout
    mainLayout = mosaicLayout
    isRoster = pidginRoster
    pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")

mosaicLayout = MosaicAlt M.empty

layout =
  onWorkspace imWorkspace imLayout $
  named "Mosaic" (smartBorders mosaicLayout) |||
  named "Tabs" (smartBorders tabbedLayout) |||
  named "Float" (smartBorders floatLayout)

myWorkspaces = map show [1 .. 9] ++ ["0", "-", "="]

myManageHook =
  composeAll
    [ className =? "Evolution" --> doShift mailWorkspace
    , className =? "Pidgin" --> doShift imWorkspace
    ]

modm = mod4Mask

myMarkup :: MarkupRenderer
myMarkup layout title workspaces _ _ = do
  H.span ! A.class_ (toValue "workspaces") $ mapM_ wsHtml workspaces
  H.span ! A.class_ (toValue "layout") $ toMarkup layout
  H.span ! A.class_ (toValue "title") $ toMarkup title
  where
    wsHtml w =
      H.span ! A.class_ (toValue $ unwords classes) $
      if isJust icon
        then do
          H.i ! A.class_ (toValue $ "fa fa-" ++ fromJust icon) $ toMarkup ""
          H.sub $ toMarkup tag
        else toMarkup tag
      where
        classes =
          ["workspace"] ++
          ["current" | wsCurrent w] ++
          ["hidden" | wsHidden w] ++
          ["urgent" | wsUrgent w] ++ ["empty" | wsEmpty w]
        tag = wsTag w
        icon = workspaceIcon tag

maxVolume :: Double
maxVolume = 0x10000

pulseAudioDump
  :: MonadIO m
  => m [String]
pulseAudioDump = liftM lines $ runProcessWithInput "pacmd" ["dump"] ""

pulseAudioDumpLine
  :: MonadIO m
  => String -> m (Maybe String)
pulseAudioDumpLine prefix = do
  dump <- pulseAudioDump
  let filtered = filter (prefix `isPrefixOf`) dump
  return $ listToMaybe filtered

currentVolume
  :: MonadIO m
  => m Double
currentVolume = do
  volumeLine <- pulseAudioDumpLine "set-sink-volume"
  let volume =
        case volumeLine of
          Just vline -> read $ last $ words vline
          _ -> 0
  return $ volume / maxVolume

currentMute
  :: MonadIO m
  => m Bool
currentMute = do
  muteLine <- pulseAudioDumpLine "set-sink-mute"
  return $
    case muteLine of
      Just mline ->
        case last $ words mline of
          "no" -> False
          "yes" -> True
          x -> error x
      _ -> True

currentSink
  :: MonadIO m
  => m String
currentSink = do
  sinkLine <- pulseAudioDumpLine "set-sink-volume"
  return $
    case sinkLine of
      Just line -> words line !! 1
      Nothing -> "alsa_output.pci-0000_00_1b.0.analog-stereo"

setVolume
  :: MonadIO m
  => Double -> m ()
setVolume vol = do
  sink <- currentSink
  spawn $ "pacmd set-sink-volume " ++ sink ++ " " ++ show volVal
  where
    newVol = max 0 $ min 1 vol
    volVal = round $ newVol * maxVolume

setMute
  :: MonadIO m
  => Bool -> m ()
setMute mute = do
  sink <- currentSink
  spawn $ "pacmd set-sink-mute " ++ sink ++ " " ++ muteStr mute
  where
    muteStr True = "yes"
    muteStr False = "no"

raiseVolume
  :: MonadIO m
  => Double -> m ()
raiseVolume percent = do
  vol <- currentVolume
  setVolume $ vol + (percent / 100)

lowerVolume
  :: MonadIO m
  => Double -> m ()
lowerVolume = raiseVolume . negate

toggleMute
  :: MonadIO m
  => m ()
toggleMute = do
  mute <- currentMute
  setMute $ not mute

screensaver
  :: MonadIO m
  => m ()
screensaver = spawn "light-locker-command -l"

suspend
  :: MonadIO m
  => m ()
suspend = spawn "systemctl suspend"

menu
  :: MonadIO m
  => m ()
menu = spawn "rofi -combi-modi window,drun,run -show combi -modi combi -font \"Fira Mono 18\""

browser :: String
browser = "chromium"

-- Listen to monitor configuration changes, and call the screen rearranging script
listenMonitorsHook :: X ()
listenMonitorsHook = withDisplay $ \dpy -> do
  root <- asks theRoot
  io $ xrrSelectInput dpy root rrScreenChangeNotifyMask

monitorsHook :: Event -> X All
monitorsHook (RRScreenChangeNotifyEvent {}) = spawn "fix-env --delay" >> pure (All True)
monitorsHook _ = pure (All True)

extraKeys =
  [ ("<XF86Messenger>", spawn "pidgin")
  , ("<XF86ScreenSaver>", screensaver)
  , ("<XF86HomePage>", spawn browser)
  , ("<XF86Display>", spawn "fix-env")
  , ("M4-<F7>", spawn "fix-env")
  , ("<Print>", spawn "gnome-screenshot -i")
  , ("M4-<F1>", screensaver)
  , ("M4-S-<F1>", suspend)
  , ("M4-<F2>", spawn browser)
  , ("<XF86AudioRaiseVolume>", raiseVolume 5)
  , ("<XF86AudioLowerVolume>", lowerVolume 5)
  , ("<XF86AudioMute>", toggleMute)
  , ("M4-s", selectSearchBrowser browser google)
  , ("M4-o", menu)
  , ("M4-h", withFocused $ sendMessage . expandWindowAlt)
  , ("M4-l", withFocused $ sendMessage . shrinkWindowAlt)
  , ("M4-S-h", withFocused $ sendMessage . tallWindowAlt)
  , ("M4-S-l", withFocused $ sendMessage . wideWindowAlt)
  , ("M4-C-<Space>", sendMessage resetAlt)
  ]

removedKeys = ["M4-p", "M4-<Return>"]

workspaceKeys
  -- Switch/move windows to workspaces
 =
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal]
  , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]
  ] ++
  -- Switch focus/move windows between workspaces
  [ ((m .|. modm, key), f sc)
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  , (f, m) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
  ]

main = do
  client <- connectSession
  xmonad $
    withUrgencyHook NoUrgencyHook $
    desktopConfig
    { terminal = "kitty"
    , workspaces = myWorkspaces
    , handleEventHook = monitorsHook <+> handleEventHook desktopConfig <+> fullscreenEventHook
    , startupHook = listenMonitorsHook <+> startupHook desktopConfig
    , manageHook =
        myManageHook <+> fullscreenManageHook <+> manageHook desktopConfig
    , layoutHook = fullscreenFull $ desktopLayoutModifiers layout
    , logHook = dbusLogWithMarkup client myMarkup
    , modMask = modm
    } `removeKeysP`
    removedKeys `additionalKeysP`
    extraKeys `additionalKeys`
    workspaceKeys
