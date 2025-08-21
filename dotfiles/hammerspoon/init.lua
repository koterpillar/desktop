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
    hs.alert.show("Main Zoom window created")
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
  end
end

zoomFilter = hs.window.filter.new('zoom.us')
zoomFilter:subscribe(hs.window.filter.windowCreated, zoomWindowCreated)
