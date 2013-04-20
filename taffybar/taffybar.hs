import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTFBS
import Data.Char
import Data.List
import Data.List.Utils
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Read

import DBus (toVariant, fromVariant, Signal(..), signal, parseObjectPath, parseInterfaceName, parseMemberName)
import DBus.Client (listen, matchAny, MatchRule(..), connectSession, emit, Client)

import Graphics.UI.Gtk hiding (Signal)
import Graphics.UI.Gtk.WebKit.WebView

import Numeric

import System.Directory

import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigDir, getUserConfigFile)

import System.Exit

import System.Gnome.GConf

import System.Information.CPU

import System.Process

import System.Taffybar
import System.Taffybar.Battery
import System.Taffybar.Systray
import System.Taffybar.Widgets.PollingBar

-- Hex colors
hexColor :: (Fractional a, Eq a) => String -> (a, a, a)
hexColor ['#', r1, r2, g1, g2, b1, b2] = rgb (hc [r1, r2], hc [g1, g2], hc [b1, b2]) where
                                         hc = fst . head . readHex

rgb :: Fractional a => (a, a, a) -> (a, a, a)
rgb (x, y, z) = (norm x, norm y, norm z)
                where norm a = a / 255

bgColor = hexColor "#FFE8C9"

batteryConfig = defaultBatteryConfig { barColor = batteryColor
                                     , barBackgroundColor = bgColor
                                     , barBorderColor = (0, 0, 0)
                                     , barPadding = 3
                                     } where batteryColor pct
                                                | pct < 0.1 = (1, 0, 0)
                                                | pct < 0.9 = (0.1, 0.1, 0.1)
                                                | otherwise = (0, 0, 0)

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) $ output

gnomeBackgroundUrl :: IO String
gnomeBackgroundUrl = gsettingsGet "org.gnome.desktop.background" "picture-uri"

-- TODO: instead of templating, send JavaScript events
htmlDataMap :: IO (M.Map String String)
htmlDataMap = do
    background <- gnomeBackgroundUrl
    return $ M.fromList [ ("background", background)
                        ]

formatHtml :: IO String
formatHtml = do
    htmlFile <- getUserConfigFile "taffybar" "index.html"
    html <- readFile htmlFile
    dataMap <- htmlDataMap
    return $ M.foldrWithKey replaceMapItem html dataMap
        where replaceMapItem k v = replace ("{{ " ++ k ++ " }}") v

setupWebkitLog :: WebView -> IO ()
setupWebkitLog w = do
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = parseObjectPath "/org/xmonad/Log"
                           , matchInterface = parseInterfaceName "org.xmonad.Log"
                           , matchMember = parseMemberName "Update"
                           }

    baseDir <- getUserConfigDir "taffybar"
    html <- formatHtml
    webViewLoadHtmlString w html ("file://" ++ baseDir)

    client <- connectSession

    listen client matcher $ callback w

escapeQuotes :: String -> String
escapeQuotes = replace "'" "\\'" . replace "\\" "\\\\"

callback :: WebView -> Signal -> IO ()
callback wk sig = do
    let [bdy] = signalBody sig
        Just status = fromVariant bdy
    postGUIAsync $ do
        Just disp <- displayGetDefault
        screen <- displayGetScreen disp $ screenNumber taffybarConfig
        sw <- screenGetWidth screen
        widgetSetSizeRequest wk sw (barHeight taffybarConfig)
        webViewExecuteScript wk $ "window.setStatus && setStatus('" ++ escapeQuotes status ++ "')"

xmonadWebkitLogNew :: IO Widget
xmonadWebkitLogNew = do
    l <- webViewNew
    on l realize $ setupWebkitLog l
    widgetShowAll l
    return (toWidget l)

taffybarConfig = defaultTaffybarConfig { startWidgets = [ log ]
                                       , endWidgets   = [ tray, battery ]
                                       }
    where log = xmonadWebkitLogNew
          tray = systrayNew
          battery = batteryBarNew batteryConfig 10

main = defaultTaffybar taffybarConfig
