local wezterm = require 'wezterm';
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
      "#FFFEFE"
    },
    brights = {
      "#000000",
      "#C82828",
      "#708B00",
      "#E9B600",
      "#4170AE",
      "#8958A7",
      "#3D999F",
      "#FFFEFE"
    },
    tab_bar = {
      background = "#dbddd9",
      active_tab = {
        bg_color = "#5f87af",
        fg_color = "white"
      },
      inactive_tab = {
        bg_color = "#8e908c",
        fg_color = "white"
      }
    }
  },
  window_decorations = "NONE",
  window_padding = {
    left = 8,
    right = 8,
    top = 8,
    bottom = 8
  },

  -- Keyboard and mouse
  use_ime = true,

  -- Behavior
  scrollback_lines = 10000,
} 
