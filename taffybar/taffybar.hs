import System.Taffybar
import System.Taffybar.Battery
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.XMonadLog
import System.Taffybar.Widgets.PollingGraph
import System.Information.CPU

main = do
  let clock = textClockNew Nothing "%a %b %_d %H:%M" 1
      log = xmonadLogNew
      tray = systrayNew
      note = notifyAreaNew defaultNotificationConfig
      battery = batteryBarNew defaultBatteryConfig 10
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ log, note ]
                                        , endWidgets = [ tray, clock, battery ]
                                        }
