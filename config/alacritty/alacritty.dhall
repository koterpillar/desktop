-- https://github.com/alacritty/alacritty/blob/master/alacritty.yml

let OS = ./common/OS.dhall

let os = env:DHALL_OS

let font = "Fira Mono"

in
{ window =
  { decorations = "none"
  , dynamic_padding = True
  , padding = { x = 2, y = 2 }
  , startup_mode = "Maximized"
  }
, colors =
  { bright =
    { black = "0x000000"
    , blue = "0x4170AE"
    , cyan = "0x3D999F"
    , green = "0x708B00"
    , magenta = "0x8958A7"
    , red = "0xC82828"
    , white = "0xFFFEFE"
    , yellow = "0xE9B600"
    }
  , cursor = { cursor = "0x4c4c4c", text = "0xffffff" }
  , normal =
    { black = "0x000000"
    , blue = "0x4171AE"
    , cyan = "0x3E999F"
    , green = "0x718C00"
    , magenta = "0x8959A8"
    , red = "0xC82828"
    , white = "0xFFFEFE"
    , yellow = "0xEAB700"
    }
  , primary = { background = "0xffffff", foreground = "0x4d4d4c" }
  }
, dynamic_title = True
, font =
  { bold.family = font
  , italic.family = font
  , normal.family = font
  , size = merge { Linux = 10, Macos = 12 } os
  }
, key_bindings =
  [ { action = "Paste", key = "V", mods = "Control|Shift" }
  , { action = "Copy", key = "C", mods = "Control|Shift" }
  , { action = "ReceiveChar", key = "Insert", mods = "Shift" }
  , { action = "ResetFontSize", key = "Key0", mods = "Control" }
  , { action = "IncreaseFontSize", key = "Equals", mods = "Control" }
  , { action = "IncreaseFontSize", key = "Add", mods = "Control" }
  , { action = "DecreaseFontSize", key = "Subtract", mods = "Control" }
  , { action = "DecreaseFontSize", key = "Minus", mods = "Control" }
  , { action = "ReceiveChar", key = "Key0", mods = "Command" }
  , { action = "ReceiveChar", key = "Equals", mods = "Command" }
  , { action = "ReceiveChar", key = "Add", mods = "Command" }
  , { action = "ReceiveChar", key = "Minus", mods = "Command" }
  , { action = "ReceiveChar", key = "K", mods = "Command" }
  , { action = "ReceiveChar", key = "K", mods = "Command" }
  , { action = "ReceiveChar", key = "V", mods = "Command" }
  , { action = "ReceiveChar", key = "C", mods = "Command" }
  , { action = "ReceiveChar", key = "H", mods = "Command" }
  , { action = "ReceiveChar", key = "M", mods = "Command" }
  , { action = "ReceiveChar", key = "Q", mods = "Command" }
  , { action = "ReceiveChar", key = "W", mods = "Command" }
  , { action = "ReceiveChar", key = "F", mods = "Command|Control" }
  ]
, mouse.url.modifiers = "Control"
, shell =
  { args = [ "tmux", "new", "-A", "-s", "alacritty" ]
  , program = "/usr/bin/env"
  }
}