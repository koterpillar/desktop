import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTFBS
import Data.Char
import Data.List
import Data.List.Split
import Data.List.Utils
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Read

import DBus (toVariant, fromVariant, Signal(..), signal, parseObjectPath, parseInterfaceName, parseMemberName)
import DBus.Client (listen, matchAny, MatchRule(..), connectSession, emit, Client)

import Graphics.UI.Gtk hiding (Signal)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings

import Numeric

import System.Directory

import System.Environment.XDG.BaseDir (getUserCacheFile, getUserConfigDir, getUserConfigFile)

import System.Exit

import System.Gnome.GConf

import System.Information.CPU

import System.Process

import System.Taffybar
import System.Taffybar.Systray

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

-- TODO: instead of templating, send JavaScript events
htmlDataMap :: IO (M.Map String String)
htmlDataMap = do
    Just disp <- displayGetDefault
    screen <- displayGetScreen disp $ screenNumber taffybarConfig
    (Rectangle _ _ monitor_width monitor_height) <-
        screenGetMonitorGeometry screen $ monitorNumber taffybarConfig
    return $ M.fromList [ ("monitor.width",  show monitor_width)
                        , ("monitor.height", show monitor_height)
                        ]

formatHtml :: IO String
formatHtml = do
    htmlFile <- getUserConfigFile "taffybar" "index.html"
    html <- readFile htmlFile
    dataMap <- htmlDataMap
    return $ M.foldrWithKey replaceMapItem html dataMap
        where replaceMapItem k = replace ("{{ " ++ k ++ " }}")

gsettingsPrefix = "gsettings:"

setupWebkitLog :: WebView -> IO ()
setupWebkitLog wk = do
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = parseObjectPath "/org/xmonad/Log"
                           , matchInterface = parseInterfaceName "org.xmonad.Log"
                           , matchMember = parseMemberName "Update"
                           }

    baseDir <- getUserConfigDir "taffybar"
    html <- formatHtml
    webViewLoadHtmlString wk html ("file://" ++ baseDir)

    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    on wk resourceRequestStarting $ \_ _ nreq _ -> case nreq of
        Nothing -> return ()
        (Just req) -> do
            uri_ <- networkRequestGetUri req
            case uri_ of
                Nothing -> return ()
                Just uri -> when (gsettingsPrefix `isPrefixOf` uri) $ do
                    let path = drop (length gsettingsPrefix) uri
                    let [schema, key] = splitOn "/" path
                    setting <- gsettingsGet schema key
                    networkRequestSetUri req $
                        "data:text/plain," ++ setting

    client <- connectSession

    listen client matcher $ callback wk

escapeQuotes :: String -> String
escapeQuotes = replace "'" "\\'" . replace "\\" "\\\\"

callback :: WebView -> Signal -> IO ()
callback wk sig = do
    let [bdy] = signalBody sig
        Just status = fromVariant bdy
    postGUIAsync $ do
        Just disp <- displayGetDefault
        screen <- displayGetScreen disp $ screenNumber taffybarConfig
        (Rectangle _ _ sw _) <- screenGetMonitorGeometry screen $
            monitorNumber taffybarConfig
        widgetSetSizeRequest wk sw (barHeight taffybarConfig)
        webViewExecuteScript wk $ "window.setStatus && setStatus('" ++ escapeQuotes status ++ "')"

xmonadWebkitLogNew :: IO Widget
xmonadWebkitLogNew = do
    l <- webViewNew
    on l realize $ setupWebkitLog l
    widgetShowAll l
    return (toWidget l)

taffybarConfig = defaultTaffybarConfig { startWidgets = [ log ]
                                       , endWidgets   = [ tray ]
                                       }
    where log = xmonadWebkitLogNew
          tray = systrayNew

main = defaultTaffybar taffybarConfig
