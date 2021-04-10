local wezterm = require 'wezterm';

local TAB_BAR_BACKGROUND = "#dbddd9";
local TAB_BAR_FOREGROUND = "white";
local TAB_BAR_CONTENT = "#8e908c";
local TAB_BAR_HOVER = "#8e8db0";
local TAB_BAR_ACTIVE = "#5f87af";

function tab_style_left(bg_color)
  return wezterm.format({
    {Background = {Color=TAB_BAR_BACKGROUND}},
    {Foreground = {Color=bg_color}},
    {Text = utf8.char(0x2590)},
  }) .. " ";
end
function tab_style_right(bg_color)
  return " " .. wezterm.format({
    {Background = {Color=TAB_BAR_BACKGROUND}},
    {Foreground = {Color=bg_color}},
    {Text = utf8.char(0x258C)},
  });
end

local TAB_STYLE_LEFT = tab_style_left(TAB_BAR_CONTENT);
local TAB_STYLE_RIGHT = tab_style_right(TAB_BAR_CONTENT);

local WINDOW_PADDING = 8;

return {
  -- Appearance
  font = wezterm.font("Fira Mono"),
  font_size = 10.0,
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
  window_decorations = "NONE",
  window_padding = {
    left = WINDOW_PADDING,
    right = WINDOW_PADDING,
    top = WINDOW_PADDING,
    bottom = WINDOW_PADDING,
  },
  enable_scroll_bar = true,
  show_tab_index_in_tab_bar = false,
  tab_bar_style = {
    active_tab_left = tab_style_left(TAB_BAR_ACTIVE),
    active_tab_right = tab_style_right(TAB_BAR_ACTIVE),
    inactive_tab_left = TAB_STYLE_LEFT,
    inactive_tab_right = TAB_STYLE_RIGHT,
    inactive_tab_hover_left = TAB_STYLE_LEFT,
    inactive_tab_hover_right = TAB_STYLE_RIGHT,
    new_tab_left = TAB_STYLE_LEFT,
    new_tab_hover_left = TAB_STYLE_LEFT,
  },

  -- Keyboard and mouse
  use_ime = true,
  keys = {
    { key = "PageUp", mods = "CTRL", action = wezterm.action{ ActivateTabRelative=-1 } },
    { key = "PageDown", mods = "CTRL", action = wezterm.action{ ActivateTabRelative=1 } },
    { key = "E", mods = "CTRL|SHIFT", action = wezterm.action{ SplitVertical={ domain="CurrentPaneDomain" } } },
    { key = "O", mods = "CTRL|SHIFT", action = wezterm.action{ SplitHorizontal={ domain="CurrentPaneDomain" } } },
    { key = "H", mods = "SUPER", action = "Nop" },
  },

  -- Behavior
  scrollback_lines = 10000,
} 
