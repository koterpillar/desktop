local wezterm = require 'wezterm';

local TAB_BAR_BACKGROUND = "#dbddd9";
local TAB_BAR_FOREGROUND = "white";
local TAB_BAR_CONTENT = "#8e908c";
local TAB_BAR_HOVER = "#8e8db0";
local TAB_BAR_ACTIVE = "#5f87af";

function tab_style_left(bg_color)
  return {
    {Background = {Color=TAB_BAR_BACKGROUND}},
    {Foreground = {Color=bg_color}},
    {Text = utf8.char(0x2590)},
    {Background = {Color=TAB_BAR_CONTENT}},
    {Text = " "},
  }
end

local WINDOW_PADDING = 8;

local IS_MACOS = not not string.find(wezterm.target_triple, "apple");

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
  local tab_color = TAB_BAR_CONTENT
  if tab.is_active then
    tab_color = TAB_BAR_ACTIVE
  elseif hover then
    tab_color = TAB_BAR_HOVER
  end

  return {
    {Background = {Color=TAB_BAR_BACKGROUND}},
    {Foreground = {Color=tab_color}},
    {Text = utf8.char(0x2590)},
    {Background = {Color=tab_color}},
    {Foreground = {Color=TAB_BAR_FOREGROUND}},
    {Text = " "},
    {Text = tab.active_pane.title},
    {Text = " "},
    {Background = {Color=TAB_BAR_BACKGROUND}},
    {Foreground = {Color=tab_color}},
    {Text = utf8.char(0x258C)},
  }
end)

return {
  -- Appearance
  font = wezterm.font("Fira Mono"),
  font_size = IS_MACOS and 12.0 or 10.0,
  colors = {
    background = "#ffffff",
    foreground = "#4d4d4c",
    ansi = {
      "#000000",
      "#C82828",
      "#718C00",
      "#EAB700",
      "#4171AE",
      "#8959A8",
      "#3E999F",
      "#FFFEFE",
    },
    brights = {
      "#000000",
      "#C82828",
      "#708B00",
      "#E9B600",
      "#4170AE",
      "#8958A7",
      "#3D999F",
      "#FFFEFE",
    },
    tab_bar = {
      background = TAB_BAR_BACKGROUND,
      active_tab = {
        bg_color = TAB_BAR_ACTIVE,
        fg_color = TAB_BAR_FOREGROUND,
      },
      inactive_tab = {
        bg_color = TAB_BAR_CONTENT,
        fg_color = TAB_BAR_FOREGROUND,
      },
      inactive_tab_hover = {
        bg_color = TAB_BAR_HOVER,
        fg_color = TAB_BAR_FOREGROUND,
        intensity = "Bold",
      },
    },
    scrollbar_thumb = TAB_BAR_CONTENT,
  },
  inactive_pane_hsb = {
    saturation = 0.9,
    brightness = 0.97,
  },
  window_decorations = "RESIZE",
  window_padding = {
    left = WINDOW_PADDING,
    right = WINDOW_PADDING,
    top = WINDOW_PADDING,
    bottom = WINDOW_PADDING,
  },
  enable_scroll_bar = true,
  show_tab_index_in_tab_bar = false,
  tab_bar_style = {
    new_tab_left = wezterm.format(tab_style_left(TAB_BAR_CONTENT)),
    new_tab_hover_left = wezterm.format(tab_style_left(TAB_BAR_HOVER)),
  },
  tab_max_width = 100,

  -- Keyboard and mouse
  use_ime = true,
  keys = {
    { key = "PageUp", mods = "CTRL", action = wezterm.action{ ActivateTabRelative=-1 } },
    { key = "PageDown", mods = "CTRL", action = wezterm.action{ ActivateTabRelative=1 } },
    { key = "E", mods = "CTRL|SHIFT", action = wezterm.action{ SplitVertical={ domain="CurrentPaneDomain" } } },
    { key = "O", mods = "CTRL|SHIFT", action = wezterm.action{ SplitHorizontal={ domain="CurrentPaneDomain" } } },
    { key = "W", mods = "CTRL|SHIFT", action = wezterm.action{ CloseCurrentPane={ confirm=true } } },
    { key = "H", mods = "SUPER", action = "Nop" },
  },

  -- Behavior
  scrollback_lines = 10000,
} 
