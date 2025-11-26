require("hs.ipc")

function realConfigPath()
  path = hs.configdir .. "/init.lua"
  return hs.fs.symlinkAttributes(path).target
end

configWatcher = hs.pathwatcher.new(realConfigPath(), hs.reload):start()

function secondaryScreen()
  screens = hs.screen.allScreens()
  if #screens == 2 then
    for _, screen in pairs(screens) do
      if screen:name() == "Built-in Retina Display" then
        return screen
      end
    end
  end
  return nil
end

widgetMarginX = 78
widgetMarginY = 41
widgetSpacing = 15
widgetSizeX = 345

function zoomWindowCreated(win, app, event)
  if win:title() == "Zoom Workplace" then
    target = secondaryScreen()
    if target then
      win:moveToScreen(target)
      local screenFrame = target:frame()
      local newRect = hs.geometry.copy(screenFrame)
      newRect.x = screenFrame.x + widgetMarginX + widgetSizeX + widgetSpacing
      newRect.y = screenFrame.y + widgetMarginY
      newRect.x2 = screenFrame.x2 - widgetMarginX
      newRect.y2 = screenFrame.y2 - widgetMarginY
      win:setFrame(newRect)
    end
  elseif win:title() == "zoom floating video window" then
    target = secondaryScreen()
    if target then
      win:moveToScreen(target)
      local screenFrame = target:frame()
      local newRect = hs.geometry.copy(screenFrame)
      newRect.x = screenFrame.x2 - win:frame().w
      newRect.y = screenFrame.y
      newRect.w = win:frame().w
      newRect.h = win:frame().h
      win:setFrame(newRect)
    end
  end
end

zoomFilter = hs.window.filter.new('zoom.us')
zoomFilter:subscribe(hs.window.filter.windowCreated, zoomWindowCreated)

function refresh()
  local zoomApp = hs.application.find('zoom.us')
  if not zoomApp then
    return
  end

  for _, win in pairs(zoomApp:allWindows()) do
    zoomWindowCreated(win, zoomApp, nil)
  end
end
