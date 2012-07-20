import System.Taffybar
import System.Taffybar.Battery
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.Widgets.PollingBar

import System.Information.CPU

import System.CurrentLocale

import Data.Char
import Data.List
import Data.Maybe

import Numeric

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

main = do
  locale <- currentLocale
  let clock = textClockNew (Just locale) "%a %b %_d %H:%M" 1
      log = xmonadLogNew
      tray = systrayNew
      battery = batteryBarNew batteryConfig 10
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ log ]
                                        , endWidgets = [ tray, clock, battery ]
                                        }
