require("hs.ipc")

function realConfigPath()
  path = hs.configdir .. "/init.lua"
  return hs.fs.symlinkAttributes(path).target
end

configWatcher = hs.pathwatcher.new(realConfigPath(), hs.reload):start()

function orderedScreens()
  local screens = hs.screen.allScreens()
  local sorted = {}

  for _, screen in pairs(screens) do
    table.insert(sorted, screen)
  end

  table.sort(sorted, function(a, b)
    local af = a:fullFrame()
    local bf = b:fullFrame()
    return (af.w * af.h) > (bf.w * bf.h)
  end)

  return sorted
end

widgetMarginX = 78
widgetMarginY = 41
widgetSpacing = 15
widgetSizeX = 345

function zoomWindowCreated(win, app, event)
  if win:title() == "Zoom Workplace" then
    target = orderedScreens()[2]
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
    target = orderedScreens()[2]
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

function mainFullScreen(win)
  local target = orderedScreens()[1]
  if target then
    win:moveToScreen(target)
    win:setFrame(target:frame())
  end
end

function codeWindowCreated(win, app, event)
  mainFullScreen(win)
end

codeFilter = hs.window.filter.new('Code')
codeFilter:subscribe(hs.window.filter.windowCreated, codeWindowCreated)

function refresh()
  local zoomApp = hs.application.get('zoom.us')
  if zoomApp then
    for _, win in pairs(zoomApp:allWindows()) do
      zoomWindowCreated(win, zoomApp, nil)
    end
  end

  local codeApp = hs.application.get('Code')
  if codeApp then
    for _, win in pairs(codeApp:allWindows()) do
      codeWindowCreated(win, codeApp, nil)
    end
  end
end
