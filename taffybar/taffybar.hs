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

import System.Exit

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

effects :: [String]
effects = [ "-resize", "1920x100000"
          , "-crop", "10000x25+0+0"
          , "-size", "10000x25", "xc:white", "-compose", "blend", "-define", "compose:args=20,80", "-composite"
          ]

getFehBackground :: IO (Maybe String)
getFehBackground = do
    home <- getHomeDirectory
    let fehbg = home ++ "/.fehbg"
    fehbgExists <- doesFileExist fehbg
    if fehbgExists
        then do
            fehbgContent <- readFile fehbg
            let backgroundFile = takeWhile ('\'' /=) $ tail $ dropWhile ('\'' /=) fehbgContent
            backgroundExists <- doesFileExist backgroundFile
            if backgroundExists
                then return $ Just backgroundFile
                else return Nothing
        else return Nothing

copyBackground :: IO ()
copyBackground = do
    fehbg <- getFehBackground
    case fehbg of
        Nothing -> return ()
        Just background -> do
            home <- getHomeDirectory
            let taffybarBackground = home ++ "/.config/taffybar/background-crop.jpeg"
            backgroundCacheExists <- doesFileExist taffybarBackground
            when backgroundCacheExists $ removeFile taffybarBackground
            (exitcode, _, _) <- readProcessWithExitCode
                                    "convert"
                                    ([background] ++ effects ++ [taffybarBackground])
                                    ""
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
