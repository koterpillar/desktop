import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTFBS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Read

import Numeric

import System.CurrentLocale

import System.Directory

import System.Gnome.GConf

import System.Information.CPU

import System.Process

import System.Taffybar
import System.Taffybar.Battery
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.XMonadLog

-- Hex colors
hexColor :: (Fractional a, Eq a) => String -> (a, a, a)
hexColor ['#', r1, r2, g1, g2, b1, b2] = rgb (hc [r1, r2], hc [g1, g2], hc [b1, b2]) where
                                         hc = fst . head . readHex

rgb :: Fractional a => (a, a, a) -> (a, a, a)
rgb (x, y, z) = (norm x, norm y, norm z)
                where norm a = a / 255

menuColor = hexColor "#DFD7CF"
widgetColor = hexColor "#F2F1F0"

bgColor = hexColor "#FFE8C9"

batteryConfig = defaultBatteryConfig { barColor = batteryColor
                                     , barBackgroundColor = bgColor
                                     , barBorderColor = (0, 0, 0)
                                     , barPadding = 3
                                     } where batteryColor pct
                                                | pct < 0.1 = (1, 0, 0)
                                                | pct < 0.9 = (0.1, 0.1, 0.1)
                                                | otherwise = (0, 0, 0)

urlToFile :: String -> String
urlToFile url | urlPrefix == prefix = unescape escapedPath
              | otherwise = error "url must start with " ++ prefix
    where (urlPrefix, escapedPath) = splitAt (length prefix) url
          prefix = "file://"

unescape :: String -> String
unescape str = UTFBS.toString $ BS.pack $ unescapeBS str
    where unescapeBS [] = []
          unescapeBS ('%':c1:c2:rest) = (unescapeChar (c1:c2:[])):unescapeBS rest
          unescapeBS (x:rest) = (BS.unpack $ UTFBS.fromString $ [x]) ++ unescapeBS rest

-- unescapeChar :: String -> Char
unescapeChar str = fst $ fromRight result
    where result = hexadecimal $ Text.pack str
          fromRight (Left _) = error "invalid number"
          fromRight (Right x) = x

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) $ output

effects :: [String]
effects = [ "-resize", "1920x100000"
          , "-crop", "10000x25+0+0"
          , "-size", "10000x25", "xc:white", "-compose", "blend", "-define", "compose:args=20,80", "-composite"
          ]

copyBackground :: IO ()
copyBackground = do
    backgroundUrl <- gsettingsGet "org.gnome.desktop.background" "picture-uri"
    let desktopBackground = urlToFile backgroundUrl
    home <- getHomeDirectory
    let taffybarBackground = home ++ "/.config/taffybar/background-crop.jpeg"
    backgroundExists <- doesFileExist taffybarBackground
    when backgroundExists $ removeFile taffybarBackground
    readProcess "convert" ([desktopBackground] ++ effects ++ [taffybarBackground]) ""
    return ()

main = do
    copyBackground
    locale <- currentLocale
    let clock = textClockNew (Just locale) "%a %b %_d %H:%M" 1
        log = xmonadLogNew
        tray = systrayNew
        battery = batteryBarNew batteryConfig 10
    defaultTaffybar defaultTaffybarConfig { startWidgets = [ log ]
                                          , endWidgets = [ tray, clock, battery ]
                                          }
