import System.Taffybar
import System.Taffybar.Battery
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.Widgets.PollingBar

import System.Information.CPU

rgb :: Fractional a => (a, a, a) -> (a, a, a)
rgb (x, y, z) = (norm x, norm y, norm z)
                where norm a = a / 255

menuColor = rgb (223, 215, 207)
widgetColor = rgb (242, 241, 240)

batteryConfig = defaultBatteryConfig { barColor = batteryColor
                                     , barBackgroundColor = menuColor
                                     , barBorderColor = (0, 0, 0)
                                     , barPadding = 3
                                     } where batteryColor pct
                                                | pct < 0.1 = (1, 0, 0)
                                                | pct < 0.9 = (0.1, 0.1, 0.1)
                                                | otherwise = (0, 0, 0)

main = do
  let clock = textClockNew Nothing "%a %b %_d %H:%M" 1
      log = xmonadLogNew
      tray = systrayNew
      note = notifyAreaNew defaultNotificationConfig
      battery = batteryBarNew batteryConfig 10
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ log, note ]
                                        , endWidgets = [ tray, clock, battery ]
                                        }
